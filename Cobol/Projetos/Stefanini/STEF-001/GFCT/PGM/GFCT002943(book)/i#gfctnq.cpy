      *****************************************************************
      * NOME DA INC - I#GFCTNQ                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO  - DETALHE DEFERIMENTO   *
      *               COMUNICACAO WEB X IMS - SAIDA - GFCT0888        *
      * TAMANHO     - 1010                                            *
      * GFCTNQ-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNQ-SAIDA.
           03  GFCTNQ-LL                            PIC S9(004) COMP.
           03  GFCTNQ-ZZ                            PIC S9(004) COMP.
           03  GFCTNQ-TRANSACAO                     PIC  X(009).
           03  GFCTNQ-FUNCAO                        PIC  X(008).
           03  GFCTNQ-DADOS-RST.
               05  GFCTNQ-QTDE-OCOR                 PIC  9(003).
               05  GFCTNQ-QTDE-TOT-REG              PIC  9(006).
               05  GFCTNQ-FUNC-BDSCO                PIC  X(007).
               05  GFCTNQ-FIM                       PIC  X(001).
           03  GFCTNQ-ERRO                          PIC  9(001).
           03  GFCTNQ-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNQ-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNQ-DESC-MSG-ERRO                 PIC  X(079).
110608     03  FILLER                               PIC  X(376).
           03  GFCTNQ-DADOS.
               05  GFCTNQ-AGRUPMTO                  PIC  9(003).
ST2506*        05  GFCTNQ-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTNQ-FILIAL                    PIC  9(005).
ST2506         05  GFCTNQ-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTNQ-FILIAL                    PIC  X(004).
               05  GFCTNQ-CONTROLE                  PIC  X(002).
               05  GFCTNQ-AGENCIA                   PIC  9(005).
               05  GFCTNQ-POSTO                     PIC  9(005).
               05  GFCTNQ-SEGMENTO                  PIC  9(003).
               05  GFCTNQ-MUNICIPIO                 PIC  9(007).
               05  GFCTNQ-UF                        PIC  X(002).
               05  GFCTNQ-DESCR-AGPTO               PIC  X(030).
               05  GFCTNQ-TARIFA                    PIC  9(005).
               05  GFCTNQ-DESCR-TARIFA              PIC  X(030).
               05  GFCTNQ-TIMESTAMP                 PIC  X(026).
               05  GFCTNQ-SEQUENCIA                 PIC  9(002).
               05  GFCTNQ-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTNQ-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTNQ-DIA-AGEND                 PIC  9(002).
               05  GFCTNQ-FRANQUIA                  PIC  9(003).
               05  GFCTNQ-PERIODO                   PIC  9(001).
               05  GFCTNQ-DIA-SEMANA                PIC  9(001).
      * 1 = DOMINGO
      * 2 = SEGUNDA
      * 3 = TERCA
      * 4 = QUARTA
      * 5 = QUINTA
      * 6 = SEXTA
      * 7 = SABADO
               05  GFCTNQ-VR-NEGOCIADO              PIC  9(006)V99.
               05  GFCTNQ-PERC-DESCTO               PIC  9(003)V9(04).
               05  GFCTNQ-DIAS-RETENCAO             PIC  9(002).
               05  GFCTNQ-CTIPO-OPER-MOTVO          PIC  9(003).
               05  GFCTNQ-NOME-OPER-MOTVO           PIC  X(030).
               05  GFCTNQ-CMOTVO-JUSTF              PIC  9(002).
               05  GFCTNQ-NOME-MOTVO                PIC  X(030).
               05  GFCTNQ-RJUSTF                    PIC  X(255).
110608         05  GFCTNQ-CPER-COBR-TARIF           PIC  9(001).
110608* 1 = DIARIO
110608* 2 = SEMANAL
110608* 3 = QUINZENAL
110608* 4 = MENSAL
110608         05  GFCTNQ-CINIC-PER-COBR            PIC  9(001).
110608* 1 - DOMINGO
110608* 2 - SEGUNDA-FEIRA
110608* 3 - TERCA-FEIRA
110608* 4 - QUARTA-FEIRA
110608* 5 - QUINTA-FEIRA
110608* 6 - SEXTA-FEIRA
110608* 7 - SABADO
               05  GFCTNQ-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNQ-SUB-GRUPO-CTBIL           PIC  9(003).
