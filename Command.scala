// в этом файле описаны команды пользователя и разбор значений ввода

sealed trait Command

object Command:
  case object ShowPolicies extends Command // все политики списком
  case class ShowPolicy(name: String) extends Command // показать правила одной политики
  case class ValidatePolicy(name: String) extends Command // проверить политику на ошибки
  case class CreatePolicy(name: String, description: String) extends Command // оздать политику
  case class DeletePolicy(name: String) extends Command // удалить политику
  case class ComposePolicy(name: String, description: String, includes: Seq[String]) extends Command // создать/заменить составную политику
  case class ChangePolicyDescription(name: String, description: String) extends Command // изменить описание политики
  case class ChangePolicyIncludes(name: String, includes: Seq[String]) extends Command // изменить список вложенных политик в поставную политику

  case class AddBuiltinRule(policy: String, rule: Rule) extends Command // добавить в политику готовое правило
  case class SetRulePriority(policy: String, ruleId: String, priority: Int) extends Command // поставить приоритет правилу
  case class ToggleRule(policy: String, ruleId: String) extends Command // включить или выключить правило
  case class SetRuleReplacement(policy: String, ruleId: String, replacement: Replacement) extends Command // зменить стратегию замены у правила
  case class SetRuleDescription(policy: String, ruleId: String, description: String) extends Command // изменить описание правила
  case class DeleteRule(policy: String, ruleId: String) extends Command // удалить правило

  case class Preview(policy: String, source: String) extends Command // препросмотр замены без составления отчета
  case class Anonymize(policy: String, source: String) extends Command // сделай файл или каталог анонимным

  case object ShowLastReport extends Command // показать отчет последней обработки файлов или каталога
  case object ReloadConfig extends Command // перечитать конфиг из файла
  case object SaveConfig extends Command // сохранить конфиг

// чистые функции разбора отдельных значений пользовательского ввода
object CommandParser:
  def intOr(raw: String, default: Int): Int = // превратить строку в инт
    raw.trim.toIntOption.getOrElse(default)

  def splitList(raw: String, separator: Char): Seq[String] =
    raw.split(separator).map(_.trim).filter(_.nonEmpty).toSeq

  def splitIncludes(raw: String): Seq[String] =
    splitList(raw, ',')

  def maskChar(raw: String): Char =
    raw.trim.headOption.getOrElse('*')
