      *****************************************************************
      * NOME DA INC - I#GFCTN9                                        *
      * DESCRICAO   - FLEXIBILIZACAO AGRUPAMENTO - INCLUSAO - GFCT0408*
      *               COMUNICACAO WEB X IMS      - ENTRADA            *
      * TAMANHO     - 500                                             *
      * GFCTN9-LL   - 500                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTN9-ENTRADA.
           03  GFCTN9-LL                            PIC S9(004) COMP.
           03  GFCTN9-ZZ                            PIC S9(004) COMP.
           03  GFCTN9-TRANSACAO                     PIC  X(009).
           03  GFCTN9-FUNCAO                        PIC  X(008).
           03  GFCTN9-DADOS-RST.
               05  GFCTN9-QTDE-OCOR                 PIC  9(007).
               05  GFCTN9-QTDE-TOT-REG              PIC  9(009).
               05  GFCTN9-FUNC-BDSCO                PIC  X(007).
           03  GFCTN9-DADOS-CHAVE.
               05  GFCTN9-AGRUPAMENTO               PIC  9(003).
               05  GFCTN9-COD-TARIFA                PIC  9(005).
           03  GFCTN9-DADOS.
ST2506*        05  GFCTN9-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTN9-FILIAL                    PIC  9(005).
ST2506         05  GFCTN9-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTN9-FILIAL                    PIC  X(004).
               05  GFCTN9-CONTROLE                  PIC  X(002).
               05  GFCTN9-SEGMENTO                  PIC  9(003).
               05  GFCTN9-AGENCIA                   PIC  9(005).
               05  GFCTN9-POSTO                     PIC  9(005).
               05  GFCTN9-MUNIC                     PIC  9(007).
               05  GFCTN9-UF                        PIC  X(002).
               05  GFCTN9-CIDTFD-AGRP               PIC  X(001).
      * S = SIM
      * N = NAO
               05  GFCTN9-GRUPO-CTBIL               PIC  9(003).
               05  GFCTN9-SUB-GRUPO-CTBIL           PIC  9(003).
               05  GFCTN9-COD-CJTO-SERVC            PIC  9(003).
           03  GFCTN9-INCLUSAO.
               05  GFCTN9-TODAS-AG                  PIC  X(001).
      * S = SIM
      * N = NAO
               05  GFCTN9-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTN9-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTN9-AGENDAMENTO               PIC  X(001).
      * S = SIM
      * N = NAO (NESTE CASO DIA AGENDAMENTO = 0)
               05  GFCTN9-DIA-AGEND                 PIC  9(002).
               05  GFCTN9-DIAS-RETENCAO             PIC  9(002).
               05  GFCTN9-IND-VALOR                 PIC  9(001).
      * 1 = SEM VALOR NEGOCIADO
      * 2 = VALOR NEGOCIADO
      * 3 = PERCENTUAL DE DESCONTO
               05  GFCTN9-VR-NEGOCIADO              PIC  9(009)V99.
               05  GFCTN9-PERC-DESCTO               PIC  9(003)V9(04).
               05  GFCTN9-FRANQUIA                  PIC  9(003).
               05  GFCTN9-PERIODO                   PIC  9(001).
      * 1 = DIARIO
      * 2 = SEMANAL
      * 3 = QUINZENAL
      * 4 = MENSAL
               05  GFCTN9-CTIPO-OPER-MOTVO          PIC  9(003).
               05  GFCTN9-CMOTVO-JUSTF              PIC  9(002).
               05  GFCTN9-RJUSTF-MANUT-SERVC        PIC  X(255).
080508         05  GFCTN9-PERIODO-COBR              PIC  9(001).
080508* 1 = DIARIO
080508* 2 = SEMANAL
080508* 3 = QUINZENAL
080508* 4 = MENSAL
080508         05  GFCTN9-INIC-PER-COBR             PIC  9(001).
080508* 1 - DOMINGO
080508* 2 - SEGUNDA-FEIRA
080508* 3 - TERCA-FEIRA
080508* 4 - QUARTA-FEIRA
080508* 5 - QUINTA-FEIRA
080508* 6 - SEXTA-FEIRA
080508* 7 - SABADO
               05  GFCTN9-FIM                       PIC  X(001).
               05  GFCTN9-DIA-SEMANA                PIC  9(001).
      * 1 = DOMINGO
      * 2 = SEGUNDA
      * 3 = TERCA
      * 4 = QUARTA
      * 5 = QUINTA
      * 6 = SEXTA
      * 7 = SABADO
080508         05  FILLER                           PIC  X(087).
