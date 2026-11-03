import scala.annotation.tailrec
import scala.io.StdIn

// пользовательский интерфейс
// только чтение ввода, навигация по экранам и печать
// Все операции выполняются через Command и сервисы, которые возвращают типизированный результат; бизнес-логика не знает о консоли.

sealed trait Step

object Step:
  case object Stay extends Step // остаться на текущем экране
  case class Push(screen: Screen) extends Step // перейти на новый экран
  case object Quit extends Step // выйти из программы или вернутся на шаг назад

case class AppState(config: Config, lastReport: Option[OperationReport])
// состояние приложения

class ConsoleApp:
  // хранит состояние и запускает цикл меню
  private val configPath = ConfigLoader.DefaultPath
  // путь к конфигу

  private var state: AppState = {
    // загрузка состояния
    ConfigLoader.loadOrDefault(configPath) match
      case Right(config) =>
        AppState(config, None)
      case Left(error) =>
        println(Render.error(error))
        println("Будет использована конфигурация по умолчанию.")
        AppState(ConfigLoader.defaultConfig, None)
  }

  // запуск программы
  def start(): Unit = {
    loop(List(Screen.Main))
    // показываем экран Main
  }

  @tailrec
  private def loop(stack: List[Screen]): Unit = {
    // главный рекурсивный цикл меню

    stack match // стек открытых экранов, проверяем их наличие
      case Nil => // если стек пустой
        () // выход

      case screen :: _ => // текущий экран
        printMenu(screen) // вывод текущего экрана

        readLine("> ") match // вызываем ввод
          case None =>
            ()
            // если ввода нет - то завершаем цикл

          case Some(raw) =>
            // если пользователь что-то ввел, то
            raw.toIntOption match
              // строка ввода в чисто
              case None =>
                println("Неизвестный пункт. Введите число из меню.")
                loop(stack)
                // если не число

              case Some(0) =>
                if stack.size == 1 then ()
                else loop(stack.tail)
                // главный - выход, нет - назад

              case Some(choice) =>
                dispatch(screen, choice) match
                  case Step.Stay => loop(stack) // текущий экран
                  case Step.Push(next) => loop(next :: stack) // новый экран
                  case Step.Quit => () // выйти
  }

  // вывод менюшки для конкретного экрана
  private def printMenu(screen: Screen): Unit =
    println()
    println(s"=== ${Menu.title(screen)} ===")
    Menu.items(screen).zipWithIndex.foreach { case (label, index) =>
      println(s"${index + 1}) $label")
      // берет список меню и нумерует его
    }
    println(if screen == Screen.Main then "0) Выход" else "0) Назад")

  // что делать при выборе пункта
  private def dispatch(screen: Screen, choice: Int): Step =
    screen match // какой сейчас экран
      case Screen.Main =>
        dispatchMain(choice) // главный, главное меню
      case Screen.PolicyEditor(policy) => // редактор политики
        dispatchPolicyEditor(policy, choice)
      case Screen.RuleEditor(policy, ruleId) => // редактор правил
        dispatchRuleEditor(policy, ruleId, choice)

  // главное меню

  // обработчик выбора
  private def dispatchMain(choice: Int): Step =
    choice match
      case 1 =>
        perform(Command.ShowPolicies)

      case 2 =>
        withInput(ask("Имя политики: "))(Command.ShowPolicy.apply)

      case 3 =>
        withInput(ask("Имя политики: "))(Command.ValidatePolicy.apply)

      case 4 =>
        withInput(ask2("Имя новой политики: ", "Описание: ")) { case (name, description) =>
          Command.CreatePolicy(name, description)
        }

      case 5 =>
        ask("Имя политики для изменения: ") match
          case None =>
            Step.Quit
          case Some(name) =>
            PolicyService.find(state.config, name) match
              case Left(error) =>
                printError(error)
                Step.Stay
              case Right(policy) =>
                Step.Push(Screen.PolicyEditor(policy.name))

      case 6 =>
        addBuiltinRuleStep()

      case 7 =>
        withInput(
          ask3(
            "Имя составной политики: ",
            "Описание: ",
            "Включаемые политики через запятую: "
          )
        ) { case (name, description, includes) =>
          Command.ComposePolicy(name, description, CommandParser.splitIncludes(includes))
        }

      case 8 =>
        withInput(ask("Имя политики для удаления: "))(Command.DeletePolicy.apply)

      case 9 =>
        withInput(ask2("Политика: ", "Файл или каталог: ")) { case (policy, source) =>
          Command.Preview(policy, source)
        }

      case 10 =>
        withInput(ask2("Политика: ", "Файл или каталог: ")) { case (policy, source) =>
          Command.Anonymize(policy, source)
        }

      case 11 =>
        perform(Command.ShowLastReport)

      case 12 =>
        perform(Command.ReloadConfig)

      case 13 =>
        perform(Command.SaveConfig)

      case _ =>
        println("Неизвестный пункт. Введите число из меню.")
        Step.Stay

  // подменю политики

  private def dispatchPolicyEditor(policyName: String, choice: Int): Step =
    choice match // получаем выбор
      case 1 =>
        withInput(ask("Новое описание: "))(Command.ChangePolicyDescription(policyName, _))

      case 2 =>
        withInput(ask("Новые includes через запятую (пусто = очистить): ")) { raw =>
          Command.ChangePolicyIncludes(policyName, CommandParser.splitIncludes(raw))
        }

      case 3 =>
        editRuleStep(policyName)

      case 4 =>
        deleteRuleStep(policyName)

      case _ =>
        println("Неизвестный пункт. Введите число из меню.")
        Step.Stay

  private def editRuleStep(policyName: String): Step = {
    // выбрать правило по номеру и открыть его редактор
    PolicyService.find(state.config, policyName) match // ищем политику в конфиге
      case Left(error) =>
        printError(error)
        Step.Stay

      case Right(policy) =>
        if policy.rules.isEmpty then
          println("В политике нет правил")
          Step.Stay
          // остаемся
        else
          policy.rules.zipWithIndex.foreach { case (rule, index) =>
            println(Render.ruleChoice(index, rule))
            // проходим по правилам и к каждому добавляем номер
          }

          ask("Номер правила: ") match // спрашиваем номер правила
            case None =>
              Step.Quit
            case Some(raw) => // строка
              raw.toIntOption match
                case None =>
                  println("Неверный номер")
                  Step.Stay
                case Some(number) =>
                  val index = number - 1
                  if index < 0 || index >= policy.rules.size then
                    println("Неверный номер")
                    Step.Stay
                  else {
                    Step.Push(Screen.RuleEditor(policyName, policy.rules(index).id))
                    // если все ок - открываем редактор правила
                  }
  }

  // выбрать правило по ид и удалить его
  private def deleteRuleStep(policyName: String): Step =
    PolicyService.find(state.config, policyName) match // ищем политику
      case Left(error) => // если ее нет
        printError(error)
        Step.Stay

      case Right(policy) =>
        if policy.rules.isEmpty then
          println("В политике нет правил")
          Step.Stay
        else
          policy.rules.foreach(rule => println(s"- ${rule.id}"))
          withInput(ask("ID правила для удаления: "))(Command.DeleteRule(policyName, _))

  // подменю правила

  // обработчик меню редактора одного правила
  private def dispatchRuleEditor(policyName: String, ruleId: String, choice: Int): Step =
    choice match // смотрим выбор пользователя
      // изменить приоритет
      case 1 =>
        withInput(ask("Новый приоритет: ")) { raw =>
          Command.SetRulePriority(policyName, ruleId, CommandParser.intOr(raw, 100))
        }

        // выключить или выключить правило
      case 2 =>
        perform(Command.ToggleRule(policyName, ruleId))

        // изменить принцип замены
      case 3 =>
        readReplacement() match
          case Some(replacement) =>
            perform(Command.SetRuleReplacement(policyName, ruleId, replacement))
          case None =>
            Step.Quit

        // изменить описание правила
      case 4 =>
        withInput(ask("Новое описание правила: "))(Command.SetRuleDescription(policyName, ruleId, _))

        // ошибка при неизвестном
      case _ =>
        println("Неизвестный пункт. Введите число из меню.")
        Step.Stay

  // ввод пользователя

  // чтение строчки
  private def readLine(prompt: String): Option[String] =
    print(prompt)
    Option(StdIn.readLine()).map(_.trim)
    // читаем строку, чтобы не было null делаем обертку Option и получаем None

  // укороченный readline
  private def ask(prompt: String): Option[String] =
    readLine(prompt)

  // Тоже самое, но спрашивает 2 строки подряд
  private def ask2(first: String, second: String): Option[(String, String)] = {
    ask(first).flatMap(a => ask(second).map(b => (a, b)))
    // если ввод - None, то сразу вернеся нон, без второго запроса
  }

  // для 3-х
  private def ask3(first: String, second: String, third: String): Option[(String, String, String)] =
    ask(first).flatMap { a =>
      ask(second).flatMap { b =>
        ask(third).map(c => (a, b, c))
      }
    }

  // какой тип замены хочет пользователь?
  private def readReplacement(): Option[Replacement] =
    ask("Тип замены (constant/delete/mask/pseudonym/list): ").map(_.toLowerCase).flatMap {
      case "constant" =>
        ask("Строка замены: ").map(ConstantReplacement.apply)

      case "delete" =>
        Some(DeleteReplacement)

      case "mask" =>
        for
          startText <- ask("Сколько символов оставить в начале [2]: ")
          endText <- ask("Сколько символов оставить в конце [2]: ")
          maskText <- ask("Символ маски [*]: ")
        yield MaskReplacement(
          CommandParser.intOr(startText, 2),
          CommandParser.intOr(endText, 2),
          CommandParser.maskChar(maskText)
        )

      case "pseudonym" =>
        ask("Шаблон с {number}: ").map(PseudonymReplacement.apply)

      case "list" =>
        ask("Значения через |: ").map { raw =>
          ListReplacement(CommandParser.splitList(raw, '|'))
        }

      case _ =>
        Some(ConstantReplacement("[REDACTED]"))
        // залушка если не найден метод
    }

  // шаг в меню для добавления нового правила
  private def addBuiltinRuleStep(): Step =
    val command = for
      policyName <- ask("Политика, в которую добавить правило: ")
      ruleId <- ask("ID правила: ")
      kind <- ask("Встроенный тип (email/phone/ipv4/url/password/token): ")
      replacement <- readReplacement()
      priorityText <- ask("Приоритет [100]: ")

    yield Command.AddBuiltinRule( // если все шаги выше дали some , то создаем команду добалвения правила
      policyName, // в какую политику добавить правило
      Rule(
        id = ruleId,
        description = s"Правило $ruleId",
        matcher = BuiltinMatcher(kind),
        replacement = replacement,
        priority = CommandParser.intOr(priorityText, 100),
        enabled = true
      )
    )

    command match
      case Some(value) => perform(value)
      case None => Step.Quit

  // если ввод есть - создаем команду и выполняем
      // если нет - то нет
  private def withInput[A](read: Option[A])(build: A => Command): Step =
    read match // узнаем команду
      case Some(value) => perform(build(value))
      case None => Step.Quit

  // выполнение команд

  // выполни команду и останься в меню
  private def perform(command: Command): Step =
    execute(command)
    Step.Stay

  // посмотри какая это команда и сделай нужное действие
  private def execute(command: Command): Unit =
    command match // считываем команду
      // показать все политики
      case Command.ShowPolicies =>
        Render.policyList(state.config.policies).foreach(println)

        // показать политику по имени
      case Command.ShowPolicy(name) =>
        PolicyService.find(state.config, name) match // ищем по имени
          case Left(error) => printError(error)
          case Right(policy) => Render.policy(policy).foreach(println)

        // проверить политику (валидность)
      case Command.ValidatePolicy(name) =>
        val errors = Engine.validatePolicy(name, state.config)
        // делаем проверку политики по имени в конфиге
        if errors.isEmpty then println("Политика корректна")
        else errors.foreach(error => println("- " + Render.error(error)))

        // создать политику
      case Command.CreatePolicy(name, description) =>
        applyConfig(PolicyService.create(state.config, name, description)) {
          println(s"Политика '$name' создана")
          log(s"Создана политика: $name")
        }

        // удалить политику
      case Command.DeletePolicy(name) =>
        applyConfig(PolicyService.delete(state.config, name)) {
          println(s"Политика '$name' удалена")
          log(s"Удалена политика: $name")
        }

        // создать составную политику
      case Command.ComposePolicy(name, description, includes) =>
        applyConfig(PolicyService.compose(state.config, name, description, includes)) {
          println(s"Политика '$name' создана")
          printPolicyWarnings(name, "Предупреждения/ошибки:")
          log(s"Составлена политика: $name")
        }

        // поменять описание политики
      case Command.ChangePolicyDescription(name, description) =>
        applyConfig(PolicyService.setDescription(state.config, name, description)) {
          println("Описание обновлено")
          log(s"Политика $name изменена: описание")
        }

        // изменить вложенные политики в политике
      case Command.ChangePolicyIncludes(name, includes) =>
        applyConfig(PolicyService.setIncludes(state.config, name, includes)) {
          val errors = Engine.validatePolicy(name, state.config)
          if errors.nonEmpty then
            printPolicyWarnings(name, "Предупреждения после изменения:")
          else println("Includes обновлены")
          log(s"Политика $name изменена: includes")
        }

        // Добавить встроенное правило
      case Command.AddBuiltinRule(policyName, rule) =>
        applyConfig(PolicyService.addRule(state.config, policyName, rule)) {
          println(s"Правило '${rule.id}' добавлено в политику '$policyName'")
          log(s"Добавлено правило ${rule.id} в политику $policyName")
        }

        // поставить приоритет правилу
      case Command.SetRulePriority(policyName, ruleId, priority) =>
        applyConfig(
          PolicyService.updateRule(state.config, policyName, ruleId)(_.copy(priority = priority))
        ) {
          println(s"Правило $ruleId: приоритет теперь $priority")
          log(s"Политика $policyName изменена: правило $ruleId")
        }

        // включить/выключить правило
      case Command.ToggleRule(policyName, ruleId) =>
        PolicyService.toggleRule(state.config, policyName, ruleId) match
          case Left(error) =>
            printError(error)
          case Right((config, enabled)) =>
            commitConfig(config) {
              println(s"Правило $ruleId: активно = $enabled")
              log(s"Политика $policyName изменена: правило $ruleId")
            }

        // Изменить правило замены
      case Command.SetRuleReplacement(policyName, ruleId, replacement) =>
        applyConfig(
          PolicyService.updateRule(state.config, policyName, ruleId)(_.copy(replacement = replacement))
        ) {
          println(s"Правило $ruleId: замена изменена")
          log(s"Политика $policyName изменена: правило $ruleId")
        }

        // Изменить описание правила
      case Command.SetRuleDescription(policyName, ruleId, description) =>
        applyConfig(
          PolicyService.updateRule(state.config, policyName, ruleId)(_.copy(description = description))
        ) {
          println(s"Правило $ruleId: описание изменено")
          log(s"Политика $policyName изменена: правило $ruleId")
        }

        // удалить правило
      case Command.DeleteRule(policyName, ruleId) =>
        applyConfig(PolicyService.deleteRule(state.config, policyName, ruleId)) {
          println(s"Правило $ruleId удалено")
          log(s"Политика $policyName изменена: правило $ruleId удалено")
        }

        // препросмотр анонимизации
      case Command.Preview(policy, source) =>
        runAnonymize(policy, source, writeFiles = false)

        // анонимизация
      case Command.Anonymize(policy, source) =>
        runAnonymize(policy, source, writeFiles = true)

        // показать последний отчет
      case Command.ShowLastReport =>
        showReport()

        // перезапустить конфиг
      case Command.ReloadConfig =>
        reloadConfig()

        // сохранить конфиг
      case Command.SaveConfig =>
        saveConfig() match
          case Left(error) => printError(error)
          case Right(_) => println("Конфигурация сохранена")

  // вспомогательные операции интерфейса

  // применяет новую конфигурацию и сообщает об успехе только после записи файла на диск
  private def commitConfig(config: Config)(onSuccess: => Unit): Unit =
    state = state.copy(config = config)
    saveConfig() match
      case Left(error) =>
        printError(error)
      case Right(_) =>
        onSuccess

  //принять конфиг
  private def applyConfig(result: Either[AppError, Config])(onSuccess: => Unit): Unit =
    result match
      case Left(error) =>
        printError(error)
      case Right(config) =>
        commitConfig(config)(onSuccess)

  // провреяем политику и печатаем предупреждения, если они есть
  private def printPolicyWarnings(name: String, title: String): Unit =
    val errors = Engine.validatePolicy(name, state.config)
    if errors.nonEmpty then
      println(title)
      errors.foreach(error => println("- " + Render.error(error)))

  // запуск анонимизации через движок
  private def runAnonymize(policy: String, source: String, writeFiles: Boolean): Unit =
    Engine.anonymize(policy, source, state.config, writeFiles) match
      case Left(error) =>
        printError(error)
        log(s"Ошибка команды: ${Render.error(error)}")

      case Right(report) =>
        state = state.copy(lastReport = Some(report))
        Render.report(report).foreach(println)

        // если предпросмотр
        if !writeFiles then
          report.fileResults.headOption.foreach { result =>
            Render.filePreview(result).foreach(println)
          }
        else
          report.fileResults
            .filter(result => result.error.nonEmpty || result.skipped)
            .take(10)
            .foreach(result => println(s"Файл ${result.relativePath}: ${Render.fileStatus(result)}"))

  // показать репорт
  private def showReport(): Unit =
    state.lastReport match
      case None =>
        println("Отчётов ещё нет")

      case Some(report) =>
        Render.report(report).foreach(println)
        report.fileResults.filter(_.error.nonEmpty).foreach { result =>
          result.error.foreach { error =>
            println(s"Ошибка файла: ${result.relativePath}: ${Render.error(error)}")
          }
        }
        report.fileResults.filter(_.skipped).foreach { result =>
          println(s"Файл пропущен: ${result.relativePath}")
        }

  // перечитывание конфига с диска (для пользоватльских регулярных выражений)
  private def reloadConfig(): Unit =
    ConfigLoader.load(configPath) match
      case Right(config) =>
        state = state.copy(config = config)
        println("Конфигурация перезагружена")
        log("Конфигурация перезагружена")

      case Left(error) =>
        printError(error)
        println("Продолжаю использовать последнюю корректную версию.")

  // сохранить конфиг
  private def saveConfig(): Either[AppError, Unit] =
    ConfigLoader.save(configPath, state.config).map { _ =>
      log("Конфигурация сохранена")
    }

  // пишет сообщение в лог файл
  private def log(message: String): Unit =
    Logger.log(state.config.settings.logFile, message)

  // пишет ошибку
  private def printError(error: AppError): Unit =
    println(Render.error(error))

@main
def run(): Unit =
  val app = new ConsoleApp
  app.start()
