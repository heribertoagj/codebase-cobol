      *****************************************************************
      * NOME DA INC - I#GFCTNC                                        *
      * DESCRICAO   - FLEXIBILIZACAO  AGRUPAMENTO   - RENOVACAO/ALTER *
      *               COMUNICACAO WEB X IMS - SAIDA - GFCT0418        *
      * TAMANHO     - 1010                                            *
      * GFCTNC-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNC-SAIDA.
           03  GFCTNC-LL                            PIC S9(004) COMP.
           03  GFCTNC-ZZ                            PIC S9(004) COMP.
           03  GFCTNC-TRANSACAO                     PIC  X(009).
           03  GFCTNC-FUNCAO                        PIC  X(008).
           03  GFCTNC-QTDE-OCOR                     PIC  9(007).
           03  GFCTNC-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTNC-FUNC-BDSCO                    PIC  X(007).
           03  GFCTNC-ERRO                          PIC  9(001).
           03  GFCTNC-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNC-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNC-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTNC-FIM                           PIC  X(001).
           03  GFCTNC-DADOS-CHAVE.
               05  GFCTNC-AGRUPAMENTO               PIC  9(003).
               05  GFCTNC-COD-TARIFA                PIC  9(005).
           03  GFCTNC-DADOS.
               05  GFCTNC-CGC-CPF                   PIC  9(009).
               05  GFCTNC-FILIAL                    PIC  9(005).
               05  GFCTNC-CONTROLE                  PIC  X(002).
               05  GFCTNC-SEGMENTO                  PIC  9(003).
               05  GFCTNC-AGENCIA                   PIC  9(005).
               05  GFCTNC-POSTO                     PIC  9(005).
               05  GFCTNC-MUNIC                     PIC  9(007).
               05  GFCTNC-UF                        PIC  X(002).
               05  GFCTNC-TIMESTAMP                 PIC  X(026).
               05  GFCTNC-TODAS-AG                  PIC  X(001).
      * S = SIM
      * N = NAO
               05  GFCTNC-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTNC-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTNC-AGENDAMENTO               PIC  X(001).
      * S = SIM
      * N = NAO (NESTE CASO DIA AGENDAMENTO = 0)
               05  GFCTNC-DIA-AGEND                 PIC  9(002).
               05  GFCTNC-DIAS-RETENCAO             PIC  9(002).
               05  GFCTNC-IND-VALOR                 PIC  9(001).
      * 1 = SEM VALOR NEGOCIADO
      * 2 = VALOR NEGOCIADO
      * 3 = PERCENTUAL DE DESCONTO
               05  GFCTNC-VR-NEGOCIADO              PIC  9(009)V99.
               05  GFCTNC-PERC-DESCTO               PIC  9(003)V9(04).
               05  GFCTNC-FRANQUIA                  PIC  9(003).
               05  GFCTNC-PERIODO                   PIC  9(001).
      * 1 = DIARIO
      * 2 = SEMANAL
      * 3 = QUINZENAL
      * 4 = MENSAL
               05  GFCTNC-CTIPO-OPER-MOTVO          PIC  9(003).
               05  GFCTNC-CMOTVO-JUSTF              PIC  9(002).
               05  GFCTNC-RJUSTF-MANUT-SERVC        PIC  X(255).
               05  GFCTNC-SITUACAO-FLEX             PIC  9(002).
      * 01 = AGUARDANDO PARECER
      * 02 = DEFERIDA
      * 03 = INDEFERIDA
      * 04 = INDEFERIDA POR DECURSO DE PRAZO
      * 05 = DEFERIDA CANCELADA (FUTURA)
      * 06 = PEDIDO CANCELADO
               05  GFCTNC-DIA-SEMANA                PIC  9(001).
      * 1 = DOMINGO
      * 2 = SEGUNDA
      * 3 = TERCA
      * 4 = QUARTA
      * 5 = QUINTA
      * 6 = SEXTA
      * 7 = SABADO
080508         05  GFCTNC-PERIODO-COBR              PIC  9(001).
080508* 1 = DIARIO
080508* 2 = SEMANAL
080508* 3 = QUINZENAL
080508* 4 = MENSAL
080508         05  GFCTNC-INIC-PER-COBR             PIC  9(001).
080508* 1 - DOMINGO
080508* 2 - SEGUNDA-FEIRA
080508* 3 - TERCA-FEIRA
080508* 4 - QUARTA-FEIRA
080508* 5 - QUINTA-FEIRA
080508* 6 - SEXTA-FEIRA
080508* 7 - SABADO
               05  GFCTNC-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNC-SUB-GRUPO-CTBIL           PIC  9(003).
080508     03  FILLER                               PIC  X(483).
