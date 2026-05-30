      *****************************************************************
      * NOME DA INC - I#GFCTNB                                        *
      * DESCRICAO   - FLEXIBILIZACAO  AGRUPAMENTO   - RENOVACAO/ALTER *
      *               COMUNICACAO WEB X IMS       ENTRADA - GFCT0418  *
      * TAMANHO     - 500                                             *
      * GFCTNB-LL   - 500                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTNB-ENTRADA.
           03  GFCTNB-LL                            PIC S9(004) COMP.
           03  GFCTNB-ZZ                            PIC S9(004) COMP.
           03  GFCTNB-TRANSACAO                     PIC  X(009).
           03  GFCTNB-FUNCAO                        PIC  X(008).
           03  GFCTNB-DADOS-RST.
               05  GFCTNB-QTDE-OCOR                 PIC  9(007).
               05  GFCTNB-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNB-FUNC-BDSCO                PIC  X(007).
           03  GFCTNB-DADOS-CHAVE.
               05  GFCTNB-AGRUPAMENTO               PIC  9(003).
               05  GFCTNB-COD-TARIFA                PIC  9(005).
           03  GFCTNB-DADOS.
ST2506*        05  GFCTNB-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTNB-FILIAL                    PIC  9(005).
ST2506         05  GFCTNB-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTNB-FILIAL                    PIC  X(004).
               05  GFCTNB-CONTROLE                  PIC  X(002).
               05  GFCTNB-SEGMENTO                  PIC  9(003).
               05  GFCTNB-AGENCIA                   PIC  9(005).
               05  GFCTNB-POSTO                     PIC  9(005).
               05  GFCTNB-MUNIC                     PIC  9(007).
               05  GFCTNB-UF                        PIC  X(002).
               05  GFCTNB-TIMESTAMP                 PIC  X(026).
               05  GFCTNB-RENOVACAO.
                   07  GFCTNB-TODAS-AG              PIC  X(001).
      * S = SIM
      * N = NAO
                   07  GFCTNB-INIC-VIG-FLEX         PIC  X(010).
                   07  GFCTNB-FIM-VIG-FLEX          PIC  X(010).
                   07  GFCTNB-AGENDAMENTO           PIC  X(001).
      * S = SIM
      * N = NAO (NESTE CASO DIA AGENDAMENTO = 0)
                   07  GFCTNB-DIA-AGEND             PIC  9(002).
                   07  GFCTNB-DIAS-RETENCAO         PIC  9(002).
                   07  GFCTNB-IND-VALOR             PIC  9(001).
      * 1 = SEM VALOR NEGOCIADO
      * 2 = VALOR NEGOCIADO
      * 3 = PERCENTUAL DE DESCONTO
                   07  GFCTNB-VR-NEGOCIADO          PIC  9(009)V99.
                   07  GFCTNB-PERC-DESCTO           PIC  9(003)V9(04).
                   07  GFCTNB-FRANQUIA              PIC  9(003).
                   07  GFCTNB-PERIODO               PIC  9(001).
      * 1 = DIARIO
      * 2 = SEMANAL
      * 3 = QUINZENAL
      * 4 = MENSAL
                   07  GFCTNB-CTIPO-OPER-MOTVO      PIC  9(003).
                   07  GFCTNB-CMOTVO-JUSTF          PIC  9(002).
                   07  GFCTNB-RJUSTF-MANUT-SERVC    PIC  X(255).
080508             07  GFCTNB-PERIODO-COBR          PIC  9(001).
080508* 1 = DIARIO
080508* 2 = SEMANAL
080508* 3 = QUINZENAL
080508* 4 = MENSAL
080508             07  GFCTNB-INIC-PER-COBR         PIC  9(001).
080508* 1 - DOMINGO
080508* 2 - SEGUNDA-FEIRA
080508* 3 - TERCA-FEIRA
080508* 4 - QUARTA-FEIRA
080508* 5 - QUINTA-FEIRA
080508* 6 - SEXTA-FEIRA
080508* 7 - SABADO
               05  GFCTNB-FIM                       PIC  X(001).
               05  GFCTNB-SITUACAO                  PIC  X(001).
      * 01 = AGUARDANDO PARECER
      * 02 = DEFERIDA
      * 03 = INDEFERIDA
      * 04 = INDEFERIDA POR DECURSO DE PRAZO
      * 05 = DEFERIDA CANCELADA (FUTURA)
      * 06 = PEDIDO CANCELADO
               05  GFCTNB-DIA-SEMANA                PIC  9(001).
      * 1 = DOMINGO
      * 2 = SEGUNDA
      * 3 = TERCA
      * 4 = QUARTA
      * 5 = QUINTA
      * 6 = SEXTA
      * 7 = SABADO
               05  GFCTNB-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNB-SUB-GRUPO-CTBIL           PIC  9(003).
080508         05  FILLER                           PIC  X(064).
