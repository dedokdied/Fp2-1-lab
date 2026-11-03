import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Try

// чтение и запись текстового конфигурационного файла

object ConfigLoader:
  val DefaultPath = "config.txt" // имя файла конфигурации по умолчанию

  private val defaultSettings = AppSettings( // эталонные, базовые настройки, если в конфигурационном файле есть ошибки
    // или не хватает информации, то берем ее отсюда
    logFile = "anonymizer.log", // название файла для логов
    reportDirectory = "reports", // куда сохранять отчеты
    outputDirectory = "anonymized", // папка сохраненных анонимизорованных файлов
    defaultPolicy = "strict", // политика по умолчанию
    overwriteExistingFiles = false, // по умолчанию запрещаем перезаписывать существующие файлы
    supportedExtensions = Seq(".txt", ".log", ".md", ".csv", ".json", ".xml") // поддерживаемые форматы
  )

  private val settingKeys = Set( // допустимые имена настроек
    "logFile",
    "reportDirectory",
    "outputDirectory",
    "defaultPolicy",
    "overwriteExistingFiles",
    "supportedExtensions"
  )

  // если файла нет, создаём конфигурацию по умолчанию и сохраняем её
  def loadOrDefault(path: String): Either[AppError, Config] =
  // лево - ошибка, право - конфиг
    if fileExists(path) then load(path) // если файл есть - читаем
    else
      val config = defaultConfig // иначе берем конфиг по умолчанию и сохраняем его
      save(path, config).map(_ => config)

  // чтение файла
  def load(path: String): Either[AppError, Config] = {
    Try(Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8).asScala.toList)
    // мы пробем прочитать файл, может получится или упасть с ошибкой, для этого:
      .toEither
      .left
      .map(error => AppError.ConfigReadFailed(path, ErrorReason.fromThrowable(error)))
    // однако сейчас у нас ошибка работает как джава исключение, делаем вместо джава исключения нашу ошибку (которая лежит в errors)
      .flatMap(parse)
    // теперь если есть ошибка, то не парсим, если нету, то отдаем
  }

  // сохранение/запись файла
  def save(path: String, config: Config): Either[AppError, Unit] =
    Try {
      val target = Paths.get(path)
      if target.getParent != null then Files.createDirectories(target.getParent)
      Files.write(target, render(config).getBytes(StandardCharsets.UTF_8))
      ()
    }.toEither.left.map(error => AppError.ConfigWriteFailed(path, ErrorReason.fromThrowable(error)))

  // чистый разбор строк конфигурации
  def parse(lines: Seq[String]): Either[AppError, Config] =
    val cleaned = lines.iterator.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).toList
    // чистим строки, убираем пробелы по краям, убираем пустые строки и комментарии
    parseTopLevel(cleaned, Map.empty, Vector.empty).map { case (settings, policies) =>
      // отдаем строку разборщику, перед этим создав две пустые корзины, настройки и политики
      Config(settingsFrom(settings), policies.toSeq)
      // если разбор удался, взять корзины и собрать из них конфиг, если нет, то left и ошибка уйдет дальше
    }

  // разбор верхнего уровня (конфигурационного файла), разбирает строку и решает, что с ней делать
  private def parseTopLevel(
      lines: List[String],
      settings: Map[String, String],
      policies: Vector[Policy]
  ): Either[AppError, (Map[String, String], Vector[Policy])] =
    lines match
      case Nil =>
        Right((settings, policies))
        // строки кончились, отдаю дальше обе корзины

      case line :: rest if line.startsWith("policy ") =>
        val name = line.substring("policy ".length).trim
        parsePolicy(name, rest) match
          case Left(error) => Left(error)
          case Right((policy, remaining)) => parseTopLevel(remaining, settings, policies :+ policy)
          // строка policy - тогда зовем parsepolicy, получаем политику и остаток строк

      case line :: rest =>
        parseTopLevel(rest, updateSettings(settings, line), policies)
        // тут кладет в корзинц настроек


  // разбирает одну политику
  private def parsePolicy(
      name: String,
      lines: List[String]
  ): Either[AppError, (Policy, List[String])] =
    @tailrec
    def loop(
        rest: List[String], // остаток
        description: String, // описание
        includes: Seq[String], // вложенность
        rules: Vector[Rule] // правила
            // внутренний цикл, 4 корзины
    ): Either[AppError, (Policy, List[String])] = // ошибка или добавлоение в список политик
      rest match
        case Nil =>
          Left(AppError.ConfigMalformed(s"policy $name", "не найден завершающий end"))
          // если строки кончились, а end не было, то ошибка

        case line :: tail if line == "end" =>
          Right((Policy(name, description, includes, rules.toSeq), tail))
          // встретили end собрали политику, вернули остаток строк (строки, которые еще не разобраны)

        case line :: tail if line.startsWith("description=") =>
          loop(tail, afterEquals(line), includes, rules)
          // строка описания. берем значение после =
          // кладем в корзину и идем дальше

        case line :: tail if line.startsWith("includes=") =>
          loop(tail, description, splitList(afterEquals(line), ','), rules)
          // вложенные политики, режем по запятой и кладем в корзину

        case line :: tail if line.startsWith("rule ") =>
          val ruleId = line.substring("rule ".length).trim
          // отрезали рул и получили ид правила
          parseRule(ruleId, tail) match
            // отдаем идшник и отсток строк
            case Left(error) => Left(error)
            case Right((rule, remaining)) => loop(remaining, description, includes, rules :+ rule)
            //добавляем правило в конец корзины правил


        case _ :: tail =>
          loop(tail, description, includes, rules)
          // если строка не понятна никому, то скипаем и идем дальше

    loop(lines, "", Seq.empty, Vector.empty)
    // стартовый вызов цикла, все пустое

  // разбираем одно правило
  private def parseRule(
      id: String,
      lines: List[String]
  ): Either[AppError, (Rule, List[String])] = // вернуть ошибку или готовое правило
    @tailrec
    def collect(
        rest: List[String],
        entries: Map[String, String]
    ): Either[AppError, (Map[String, String], List[String])] = {
      // цикл, собираем поля пока не натыкаемся на end

      rest match
        case Nil =>
          Left(AppError.ConfigMalformed(s"rule $id", "не найден завершающий end"))
          // строки кончились, но end не было - ошибка

        case line :: tail if line == "end" =>
          Right((entries, tail))
          // после встречи end отдаем собранные поля и остаток строк

        case line :: tail =>
          val next = keyValue(line) match
            case Some((key, value)) => entries.updated(key, value)
            case None => entries
          collect(tail, next)
    }

    collect(lines, Map.empty).map { case (entries, remaining) =>
      // когда собрали все поля - достаем их из map

      val rule = Rule(
        id = id, // ид правила
        description = entries.getOrElse("description", ""), // описание, если нет, то пустая строка
        matcher = parseMatcher(entries.getOrElse("matcher", "")), // способ поиска
        replacement = parseReplacement(entries.getOrElse("replacement", "")), // способ замены
        priority = entries.get("priority").flatMap(_.toIntOption).getOrElse(100), // приоритет
        enabled = entries.get("enabled").forall(_ == "true") // включено или нет
      )
      (rule, remaining)
      // отдаем готовое правило и остаток строк
    }

  private def settingsFrom(values: Map[String, String]): AppSettings =
    AppSettings(
      logFile = values.getOrElse("logFile", defaultSettings.logFile), // есть ключ - берем, нет - берем по умолчанию
      reportDirectory = values.getOrElse("reportDirectory", defaultSettings.reportDirectory), // так же со вспм
      outputDirectory = values.getOrElse("outputDirectory", defaultSettings.outputDirectory),
      defaultPolicy = values.getOrElse("defaultPolicy", defaultSettings.defaultPolicy),
      overwriteExistingFiles = values.get("overwriteExistingFiles").contains("true"),
      supportedExtensions = values
        .get("supportedExtensions")
        .map(value => splitList(value, ','))
        .getOrElse(defaultSettings.supportedExtensions)
    )

  // обноление одной настройки по логике (ключ:значение)
  private def updateSettings(settings: Map[String, String], line: String): Map[String, String] =
    keyValue(line) match
      case Some((key, value)) if settingKeys.contains(key) => settings.updated(key, value)
      // если ключ известный - меняем
      case _ => settings
      // если нет - ничего не меняем

  // разбивка на ключ/значение
  private def keyValue(line: String): Option[(String, String)] =
    val index = line.indexOf('=') // позиция первого =
    if index < 0 then None // если нет =, то это не пара ключ значение
    else Some(line.substring(0, index).trim -> line.substring(index + 1).trim)
    // слева ключ, справа значение

  // задача из пары ключ значение достато значения с запятыми
  private def afterEquals(line: String): String =
    keyValue(line).map(_._2).getOrElse("")

  // задача из значений через запятые сделать список. Если проще, то из a,b,c делает Seq ("a", "b" ...... и тд
  private def splitList(value: String, separator: Char): Seq[String] =
    value.split(separator).map(_.trim).filter(_.nonEmpty).toSeq

  // говорит существует ли файл без проблем с программой
  private def fileExists(path: String): Boolean =
    Try(Files.exists(Paths.get(path))).getOrElse(false)

  // сборка текста конфигурации из объекта
  def render(config: Config): String =
    val sb = new StringBuilder
    // накапливание текста

    sb.append(s"logFile=${config.settings.logFile}\n")
    sb.append(s"reportDirectory=${config.settings.reportDirectory}\n")
    sb.append(s"outputDirectory=${config.settings.outputDirectory}\n")
    sb.append(s"defaultPolicy=${config.settings.defaultPolicy}\n")
    sb.append(s"overwriteExistingFiles=${config.settings.overwriteExistingFiles}\n")
    sb.append(s"supportedExtensions=${config.settings.supportedExtensions.mkString(",")}\n")
    // все настройки ключ=значение

    config.policies.foreach { policy =>
      sb.append(s"\npolicy ${policy.name}\n") // имя политики
      sb.append(s"description=${policy.description}\n") // описание политики
      sb.append(s"includes=${policy.includes.mkString(",")}\n") // вложенные политики

      policy.rules.foreach { rule =>
        // внутри политики все ее правила
        sb.append(s"rule ${rule.id}\n")
        sb.append(s"description=${rule.description}\n")
        sb.append(s"matcher=${matcherToText(rule.matcher)}\n")
        sb.append(s"replacement=${replacementToText(rule.replacement)}\n")
        sb.append(s"priority=${rule.priority}\n")
        sb.append(s"enabled=${rule.enabled}\n")
        sb.append("end\n")
      }

      sb.append("end\n")
    }

    sb.toString

  // текстовые формы поиска и замены используются и при записи конфига,
  // и при выводе правил пользователю.
  def matcherToText(matcher: Matcher): String = // переводим способ поиска в текст для записи в конфиг
    matcher match // тут проверяем все 4 режима поиска, если подходит, то возвращаем один из них
      case BuiltinMatcher(kind) => s"builtin:$kind"
      case RegexMatcher(pattern) => s"regex:$pattern"
      case ExactMatcher(value) => s"exact:$value"
      case DictionaryMatcher(values) => s"dict:${values.mkString("|")}"

  def replacementToText(replacement: Replacement): String = // тоже самое но способ замены в текст для записи в конфиг
    replacement match
      case ConstantReplacement(value) => s"constant:$value"
      case DeleteReplacement => "delete"
      case MaskReplacement(visibleStart, visibleEnd, maskChar) =>
        s"mask:$visibleStart:$visibleEnd:$maskChar"
      case PseudonymReplacement(template) => s"pseudonym:$template"
      case ListReplacement(values) => s"list:${values.mkString("|")}"

  private def parseMatcher(value: String): Matcher = // тут ровно наоборот, из строки делаем обьект
    if value.startsWith("builtin:") then
      BuiltinMatcher(value.stripPrefix("builtin:").trim)
    else if value.startsWith("regex:") then
      RegexMatcher(value.stripPrefix("regex:").trim)
    else if value.startsWith("exact:") then
      ExactMatcher(value.stripPrefix("exact:").trim)
    else if value.startsWith("dict:") then
      DictionaryMatcher(splitList(value.stripPrefix("dict:"), '|'))
    else
      ExactMatcher(value)

  private def parseReplacement(value: String): Replacement = // тоже самое
    if value.startsWith("constant:") then
      ConstantReplacement(value.stripPrefix("constant:"))
    else if value == "delete" then
      DeleteReplacement
    else if value.startsWith("mask:") then
      val parts = value.stripPrefix("mask:").split(':')
      val start = parts.lift(0).flatMap(_.toIntOption).getOrElse(1)
      val end = parts.lift(1).flatMap(_.toIntOption).getOrElse(1)
      val maskChar = parts.lift(2).flatMap(_.headOption).getOrElse('*')
      MaskReplacement(start, end, maskChar)
    else if value.startsWith("pseudonym:") then
      PseudonymReplacement(value.stripPrefix("pseudonym:"))
    else if value.startsWith("list:") then
      ListReplacement(splitList(value.stripPrefix("list:"), '|'))
    else
      ConstantReplacement(value)

  // конфигурация по умолчанию
  def defaultConfig: Config =
    val personalData = Policy(
      name = "personal-data",
      description = "Персональные данные",
      includes = Seq.empty,
      rules = Seq(
        Rule(
          id = "emails",
          description = "Адреса электронной почты",
          matcher = BuiltinMatcher("email"),
          replacement = PseudonymReplacement("user{number}@example.com"),
          priority = 100,
          enabled = true
        ),
        Rule(
          id = "phones",
          description = "Телефонные номера",
          matcher = BuiltinMatcher("phone"),
          replacement = ConstantReplacement("[PHONE]"),
          priority = 90,
          enabled = true
        )
      )
    )

    val credentials = Policy(
      name = "credentials",
      description = "Пароли и токены",
      includes = Seq.empty,
      rules = Seq(
        Rule(
          id = "password-fields",
          description = "Пароли",
          matcher = BuiltinMatcher("password"),
          replacement = ConstantReplacement("[REDACTED]"),
          priority = 200,
          enabled = true
        ),
        Rule(
          id = "tokens",
          description = "Токены и ключи доступа",
          matcher = BuiltinMatcher("token"),
          replacement = ConstantReplacement("[REDACTED]"),
          priority = 200,
          enabled = true
        )
      )
    )

    val networkData = Policy(
      name = "network-data",
      description = "Сетевые данные",
      includes = Seq.empty,
      rules = Seq(
        Rule(
          id = "ipv4",
          description = "IPv4 адреса",
          matcher = BuiltinMatcher("ipv4"),
          replacement = PseudonymReplacement("host{number}"),
          priority = 80,
          enabled = true
        ),
        Rule(
          id = "url",
          description = "URL",
          matcher = BuiltinMatcher("url"),
          replacement = ConstantReplacement("[URL]"),
          priority = 70,
          enabled = true
        )
      )
    )

    val strict = Policy(
      name = "strict",
      description = "Полная очистка перед публикацией",
      includes = Seq("personal-data", "credentials", "network-data"),
      rules = Seq(
        Rule(
          id = "emails",
          description = "Адреса электронной почты",
          matcher = BuiltinMatcher("email"),
          replacement = ConstantReplacement("[EMAIL]"),
          priority = 150,
          enabled = true
        )
      )
    )

    Config(defaultSettings, Seq(personalData, credentials, networkData, strict))
