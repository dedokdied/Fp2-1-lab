// файл ошибок, где описаны ожидаемые ошибки программы
// это не тексты, а типы ошибок (что именно не так)
// текст собирается отедльно

//проблемы правил
sealed trait RuleProblem

object RuleProblem:
  case object EmptyRuleId extends RuleProblem // пустое ид
  case class UnknownBuiltinKind(kind: String) extends RuleProblem // указание неищвестного встроенного типа
  case class InvalidRegex(pattern: String) extends RuleProblem // регулярное выражение не работает
  case object EmptyExactValue extends RuleProblem // для поиска по точной строке не указано значение
  case object EmptyDictionary extends RuleProblem // список слов для замены пустой
  case class NegativeMaskBounds(start: Int, end: Int) extends RuleProblem // у маски проблемные границы
  case class MissingPseudonymPlaceholder(template: String) extends RuleProblem // в шаблоне псевдонима нет списка номеров
  case object EmptyListReplacement extends RuleProblem // списка замены нету заданых значений

// общие ошибки программы
sealed trait AppError

object AppError:
  // политики
  case object EmptyPolicyName extends AppError
  case class PolicyNotFound(name: String) extends AppError
  case class PolicyAlreadyExists(name: String) extends AppError // политика с таким уменем уже есть
  case class MissingIncludedPolicy(policy: String, include: String) extends AppError // внутри лестничной политики лежит несуществующая вложенная политика
  case class CyclicPolicyInclude(name: String) extends AppError // политики включают друг друга по кругу
  // возвращаем именно ту политику, на которой был найден цикл

  // правила
  case class RuleNotFound(policy: String, ruleId: String) extends AppError // в политике нету правила с таким ид
  case class RuleAlreadyExists(policy: String, ruleId: String) extends AppError // правило с таким id в политике уже есть
  case class DuplicateRuleId(policy: String, ruleId: String) extends AppError // одинаковые ид у нескольких правил при проверке политики
  case class InvalidRule(policy: String, ruleId: String, problem: RuleProblem) extends AppError // в правиле ошибка, а проблему уточняем через проблему правила

  // конфигурация
  case class ConfigReadFailed(path: String, reason: String) extends AppError // не удалось прочитать файл конфигурации
  case class ConfigWriteFailed(path: String, reason: String) extends AppError // не удалось записать файл конфигурации
  case class ConfigMalformed(block: String, reason: String) extends AppError // файл прочитан, но внутри стуктурыне ошибки

  // пути и файлы при анонимизации
  case class InvalidPath(path: String, reason: String) extends AppError // путь некоректен, например его не суще ствует
  case class SourceNotFound(path: String) extends AppError // каталога или файла не сущестует
  case class OutputInsideSource(source: String, output: String) extends AppError // каталог вывода находится внутри каталога источника
  case class FileReadFailed(path: String, reason: String) extends AppError // не удалось прочитать файл
  case class FileWriteFailed(path: String, reason: String) extends AppError // не удалось записать анонимный файл

object ErrorReason:
  def fromThrowable(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
