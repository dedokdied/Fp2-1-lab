// операции над конфигурацией (политики и правила)
// все функции чистые: они не читают ввод, не печатают и не пишут файлы,
// а возвращают новую конфигурацию или типизированную ошибку.

object PolicyService:
  // идея в том, что мы принимает старый конфиг, а возвращаем либо ошибку, либо новый конфиг

  def find(config: Config, name: String): Either[AppError, Policy] = { // найти политику по имеи
    config.findPolicy(name).toRight(AppError.PolicyNotFound(name))
    // если есть, то передаем политику, если нет, то ошибка
  }

  def findRule(config: Config, policyName: String, ruleId: String): Either[AppError, Rule] = {
    // ищем правило внутри политики
    find(config, policyName).flatMap { policy =>
      policy.rules.find(_.id == ruleId).toRight(AppError.RuleNotFound(policyName, ruleId))
    }
  }

  def create(config: Config, name: String, description: String): Either[AppError, Config] = {
    // создать новую политику
    if name.trim.isEmpty then Left(AppError.EmptyPolicyName) // имя не пустое
    else if config.findPolicy(name).nonEmpty then Left(AppError.PolicyAlreadyExists(name)) // если в конфиге нету еще такой политики
    else Right(config.withPolicy(Policy(name, description, Seq.empty, Seq.empty)))
  }

  // составная политика, заменяет политику с таким же именем
  def compose(
             // создать или заменить составную политику
      config: Config,
      name: String,
      description: String,
      includes: Seq[String]
  ): Either[AppError, Config] =
    if name.trim.isEmpty then Left(AppError.EmptyPolicyName) // имя не пустое
    else Right(config.withPolicy(Policy(name, description, includes, Seq.empty)))
  // если политика с таким, то заменяем

  def delete(config: Config, name: String): Either[AppError, Config] = {
    // удалить политику
    find(config, name).map(_ => config.withoutPolicy(name))
    // убираем политику
  }

  def setDescription(
                    // изменить описание политики
      config: Config,
      name: String,
      description: String
  ): Either[AppError, Config] =
    find(config, name).map(policy => config.withPolicy(policy.copy(description = description)))

  def setIncludes(
                 // изменить список вложенных политик
      config: Config,
      name: String,
      includes: Seq[String]
  ): Either[AppError, Config] =
    find(config, name).map(policy => config.withPolicy(policy.copy(includes = includes)))

  def addRule(config: Config, policyName: String, rule: Rule): Either[AppError, Config] = {
    // добавить правило
    if rule.id.trim.isEmpty then Left(AppError.InvalidRule(policyName, rule.id, RuleProblem.EmptyRuleId))
      // если ид правила не пустой
    else
      find(config, policyName).flatMap { policy =>
        // политика существует
        if policy.rules.exists(_.id == rule.id) then {
          // правила с таким ид еще нет
          Left(AppError.RuleAlreadyExists(policyName, rule.id))
        } else
          Right(config.withPolicy(policy.copy(rules = policy.rules :+ rule)))
      }
  }

  def updateRule(
                // обновить правило
      config: Config,
      policyName: String,
      ruleId: String
  )(change: Rule => Rule): Either[AppError, Config] = {
    // как именно изменить правило
    find(config, policyName).flatMap { policy =>
      // если политика существует
      policy.rules.find(_.id == ruleId) match
        // найти правило
        case None =>
          Left(AppError.RuleNotFound(policyName, ruleId))
        case Some(rule) =>
          val updated = change(rule)
          // обновляем правило
          val rules = policy.rules.map(item => if item.id == ruleId then updated else item)
          Right(config.withPolicy(policy.copy(rules = rules)))
    }
  }

  def toggleRule(
                // включить или выключить правило
      config: Config,
      policyName: String,
      ruleId: String
  ): Either[AppError, (Config, Boolean)] =
    findRule(config, policyName, ruleId).flatMap { rule =>
      updateRule(config, policyName, ruleId)(_.copy(enabled = !rule.enabled))
        .map(updated => (updated, !rule.enabled))
    }

  def deleteRule(
                // удалить правило
      config: Config,
      policyName: String,
      ruleId: String
  ): Either[AppError, Config] =
    findRule(config, policyName, ruleId).flatMap { _ =>
      find(config, policyName).map { policy =>
        config.withPolicy(policy.copy(rules = policy.rules.filterNot(_.id == ruleId)))
      }
    }
