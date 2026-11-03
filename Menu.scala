// экраны меню и их пункты, навигация описана отдельным ADT, а действия
// пользователя в Command

sealed trait Screen

object Screen:
  case object Main extends Screen
  case class PolicyEditor(policy: String) extends Screen
  case class RuleEditor(policy: String, ruleId: String) extends Screen

object Menu:
  def title(screen: Screen): String =
    screen match
      case Screen.Main =>
        "Менеджер политик анонимизации"
      case Screen.PolicyEditor(policy) =>
        s"Изменение политики $policy"
      case Screen.RuleEditor(policy, ruleId) =>
        s"Правило $ruleId (политика $policy)"

  def items(screen: Screen): Seq[String] =
    screen match
      case Screen.Main =>
        Seq(
          "Показать все политики",
          "Показать правила политики",
          "Проверить политику",
          "Создать пустую политику",
          "Изменить политику",
          "Добавить встроенное правило",
          "Составить политику",
          "Удалить политику",
          "Предпросмотр обработки",
          "Анонимизировать файл/каталог",
          "Показать последний отчёт",
          "Перезагрузить конфигурацию",
          "Сохранить конфигурацию"
        )

      case Screen.PolicyEditor(_) =>
        Seq(
          "Изменить описание",
          "Задать список includes",
          "Изменить правило",
          "Удалить правило"
        )

      case Screen.RuleEditor(_, _) =>
        Seq(
          "Изменить приоритет",
          "Включить/выключить",
          "Изменить замену",
          "Изменить описание правила"
        )
