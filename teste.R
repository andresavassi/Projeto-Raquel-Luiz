rm(list = ls())
library(readxl)
library(dplyr)
library(kableExtra)
library(knitr)
library(tidyr)
library(ggplot2)
caminho <- paste0(getwd(), "/dados.xlsx")

#========================================
# TRATATIVA DOS DADOS ----
#========================================

dados <- read_xlsx(caminho, sheet = 1)
psql <- read_xlsx(caminho, sheet = 4)
ipaq <- read_xlsx(caminho, sheet = 6)
df_8 <- read_xlsx(caminho, sheet = 8)


dados <- dados[-c(1, 2), ]

library(dplyr)


dados <- dados %>%
  rename(
    data_resposta = `Carimbo de data/hora`,
    consentimento = `Ao selecionar o botão abaixo, o(a) Senhor(a) concorda em participar da pesquisa de acordo com as informações registradas neste Termo de Consentimento Livre e Esclarecido. Caso não concorde em participar, apenas feche essa página no seu navegador.\n`,
    idade = Idade,
    sexo = Sexo,
    ano_academico = `Ano acadêmico`,
    ocupacao = Ocupação,
    diagnostico_previo = `Você possui algum diagnóstico prévio? (psiquiátricos; doenças crônicas...)`,
    habito_estudo = `Como você descreve seus hábitos de estudo?`,
    atividade_extracurricular = `Você está realizando alguma atividade extracurricular atualmente?`,
    usa_gestao_tempo = `Você utiliza alguma ferramenta ou estratégia de gestão de tempo?`,
    qual_gestao_tempo = `Se respondeu sim na pergunta anterior, descreva qual (aplicativos, técnicas...).`,
    disciplina_gestao_academica = `Em que medida a disciplina de QUALIDADE DE VIDA, GESTÃO PESSOAL E DA CARREIRA MÉDICA, ofertada no 1º período, contribuiu para o seu gerenciamento das atividades acadêmicas?`,
    disciplina_gestao_fisica = `Em que medida a disciplina de QUALIDADE DE VIDA, GESTÃO PESSOAL E DA CARREIRA MÉDICA, ofertada no 1º período, contribuiu para a conciliação da sua rotina acadêmica com a prática de atividade física?`,
    disciplina_gestao_sono = `Em que medida a disciplina de QUALIDADE DE VIDA, GESTÃO PESSOAL E DA CARREIRA MÉDICA, ofertada no 1º período, contribuiu para a conciliação da sua rotina acadêmica com um sono de qualidade?`,
    mentoring_gestao_academica = `Em que medida o Programa de Mentoring, ofertado ao longo de todo o curso, contribuiu para o seu gerenciamento das atividades acadêmicas?`,
    mentoring_gestao_fisica = `Em que medida o Programa de Mentoring, ofertado ao longo de todo o curso, contribuiu para a conciliação da sua rotina acadêmica com a prática de atividade física?`,
    mentoring_gestao_sono = `Em que medida o Programa de Mentoring, ofertado ao longo de todo o curso, contribuiu para a conciliação da sua rotina acadêmica com um sono de qualidade?`,
    atletica_fisica = `Em que medida a Atlética de Medicina contribuiu para a sua prática de atividades físicas?`,
    ingresso_impacto_fisica = `O ingresso na universidade influenciou no seu hábito de praticar atividade física?`,
    reduz_fisica_prova = `Você costuma reduzir o tempo dedicado à atividade física durante as semanas de prova?`,
    reduz_sono_universidade = `Após ingressar na universidade, você percebeu uma redução na qualidade do seu sono?`,
    reduz_sono_prova = `Você costuma reduzir suas horas de sono nas semanas de provas?`,
    uso_substancias_estudo = `Nos últimos 30 dias, você utilizou alguma substância para tentar aumentar seu rendimento nos estudos, como para ficar acordado ou melhorar a concentração?`,
    hora_deitar = `Durante o último mês, quando você geralmente foi para a cama à noite? (Hora usual de deitar)`,
    latencia_sono_min = `Durante o último mês, quanto tempo (em minutos) você geralmente levou para dormir à noite? (Número de minutos)`,
    hora_levantar = `Durante o último mês, quando você geralmente levantou de manhã? (Hora usual de levantar)`,
    horas_sono_noite = `Durante o último mês, quantas horas de sono você teve por noite? (Este pode ser diferente do número de horas que você ficou na\ncama)`,
    dificuldade_adormecer = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você não conseguiu adormecer em até 30 minutos?`,
    acorda_noite = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você acordou no meio da noite ou de manhã cedo?`,
    levanta_banheiro = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você precisou levantar para ir ao banheiro?`,
    respira_dificuldade = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você não conseguiu respirar confortavelmente?`,
    tosse_ronco = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você tossiu ou roncou forte?`,
    sono_frio = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você sentiu muito frio?`,
    sono_calor = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você sentiu muito calor?`,
    sonhos_ruins = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você teve sonhos ruins?`,
    dor_sono = `Durante o último mês, com que frequência você teve dificuldade de dormir porque você teve dor?`,
    sono_outras_razoes = `Durante o último mês, você teve dificuldade para dormir por alguma(s) outra(s) razão(ões)? Se sim, por favor, descreva:`,
    sono_outras_freq = `Durante o último mês, com que frequência você teve dificuldade de dormir devido a razão descrita na pergunta anterior?`,
    sono_qualidade = `Durante o último mês, como você classificaria a qualidade do seu sono de uma maneira geral?`,
    medicamento_sono = `Durante o último mês, com que frequência você tomou medicamento (prescrito ou ‘‘por conta própria’’) para lhe ajudar a dormir?`,
    dificuldade_acordado = `No último mês, com que frequência você teve dificuldade de ficar acordado enquanto dirigia, comia ou participava de uma atividade social (festa, reunião de amigos, trabalho, estudo)?`,
    entusiasmo_problema = `Durante o último mês, quão problemático foi para você manter o entusiasmo (ânimo) para fazer as coisas (suas atividades habituais)?`,
    tem_parceiro = `Você tem um(a) parceiro [esposo(a)] ou colega de quarto?`,
    parceiro_ronco = `Se você tem um parceiro ou colega de quarto, pergunte a ele/ela com que frequência, no último mês, você teve ronco forte:`,
    parceiro_apneia = `Se você tem um parceiro ou colega de quarto, pergunte a ele/ela com que frequência, no último mês, você teve longas paradas na respiração enquanto dormia:`,
    parceiro_movimento_pernas = `Se você tem um parceiro ou colega de quarto, pergunte a ele/ela com que frequência, no último mês, você teve contrações ou puxões nas pernas enquanto você dormia:`,
    parceiro_desorientacao = `Se você tem um parceiro ou colega de quarto, pergunte a ele/ela com que frequência, no último mês, você teve episódios de desorientação ou confusão durante o sono:`,
    parceiro_outras_descr = `Se você tem um parceiro ou colega de quarto, pergunte a ele(a) se no último mês, você apresentou outras alterações (inquietações) enquanto dorme, por favor, descreva:`,
    parceiro_outras_freq = `Se você tem um parceiro ou colega de quarto, pergunte a ele(a) com que frequência, no último mês, você apresentou as outras alterações (inquietações) enquanto você dorme, descritas na questão anterior:`,
    dias_caminhada = `Em quantos dias da última semana você CAMINHOU por pelo menos 10 minutos contínuos? Seja em casa, no trabalho, como forma de transporte para ir de um lugar para outro, por lazer, por prazer ou como forma de exercício.`,
    tempo_caminhada = `Nos dias em que você caminhou por pelo menos 10 minutos contínuos, quanto tempo no total você gastou caminhando por dia?`,
    dias_moderada = `Em quantos dias da última semana você realizou atividades MODERADAS por pelo menos 10 minutos contínuos? Excluída a caminhada, considere atividades que aumentem moderamente sua respiração ou batimentos cardíacos, como nadar, dançar, aeróbico leve, vôlei recreativo, serviços domésticos...`,
    tempo_moderada = `Nos dias em que você fez essas atividades moderadas por pelo menos 10 minutos contínuos, quanto tempo no total você gastou fazendo essas atividades por dia?`,
    dias_vigorosa = `Em quantos dias da última semana você realizou atividades VIGOROSAS por pelo menos 10 minutos contínuos? Considere qualquer atividade que fez aumentar MUITO sua respiração ou batimentos cardíacos, como correr, jogar futebol, pedalar rápido na bicicleta, jogar basquete, fazer serviços domésticos pesados...`,
    tempo_vigorosa = `Nos dias em que você fez essas atividades vigorosas por pelo menos 10 minutos contínuos, quanto tempo no total você gastou fazendo essas atividades por dia?`,
    sentado_semana = `Quanto tempo no total você gasta sentado durante um dia de semana? Isso inclui o tempo ativo e de descanso, no trabalho, faculdade ou em casa. Não inclua o tempo gasto sentando durante o transporte em ônibus, trem, metrô ou carro.`,
    sentado_fds = `Quanto tempo no total você gasta sentado durante um dia de final de semana? Isso inclui o tempo ativo e de descanso, no trabalho, faculdade ou em casa. Não inclua o tempo gasto sentando durante o transporte em ônibus, trem, metrô ou carro.`,
    tem_parceiro_q10 = `10. Você tem um(a) parceiro [esposo (a)] ou colega de quarto?`
  )

dados$participantes <- df_8$Participantes


df_exc <- dados[-c(42, 43, 46, 75, 107, 108, 125, 135, 142, 256, 257), ]
ipaq1 <- ipaq[-c(42, 43, 46, 75, 107, 108, 125, 135, 142, 256, 257), ]


df_exc$psql <- psql$`Pontuação Final`
df_exc$ipaq <- ipaq1$Total
rm(ipaq1)


#========================================
# Descritivas ----
#========================================

dados %>%
  group_by(diagnostico_previo) %>%
  summarise(n = n())


tabela <- dados %>%
  group_by(sexo, ocupacao) %>%
  summarise(n = n(), .groups = 'drop') %>%
  bind_rows(
    dados %>%
      group_by(sexo) %>%
      summarise(ocupacao = "Total", n = n(), .groups = 'drop'),
    dados %>%
      group_by(ocupacao) %>%
      summarise(sexo = "Total", n = n(), .groups = 'drop'),
    dados %>%
      summarise(sexo = "Total", ocupacao = "Total", n = n())
  ) %>%
  mutate(
    porcentagem = round(
      n / max(n[sexo == "Total" & ocupacao == "Total"]) * 100,
      1
    ),
    valor_formatado = paste0(n, " (", porcentagem, "%)")
  ) %>%
  select(sexo, ocupacao, valor_formatado) %>%
  pivot_wider(names_from = ocupacao, values_from = valor_formatado) %>%
  mutate(
    sexo = factor(sexo, levels = c("Sexo Feminino", "Sexo Masculino", "Total"))
  ) %>%
  arrange(sexo) %>%
  select(sexo, `Estuda`, `Estuda e trabalha`, Total)


tabela %>%
  kable(
    align = c('l', 'c', 'c', 'c'),
    col.names = c("", "Estuda", "Estuda e trabalha", "Total")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = "striped")
