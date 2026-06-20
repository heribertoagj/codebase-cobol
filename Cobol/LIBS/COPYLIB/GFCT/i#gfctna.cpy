      *****************************************************************
      * NOME DA INC - I#GFCTNA                                        *
      * DESCRICAO   - FLEXIBILIZACAO AGRUPAMENTO - INCLUSAO - GFCT0408*
      *               COMUNICACAO WEB X IMS      - SAIDA              *
      * TAMANHO     - 1010                                            *
      * GFCTNA-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNA-SAIDA.
           03  GFCTNA-LL                            PIC S9(004) COMP.
           03  GFCTNA-ZZ                            PIC S9(004) COMP.
           03  GFCTNA-TRANSACAO                     PIC  X(009).
           03  GFCTNA-FUNCAO                        PIC  X(008).
           03  GFCTNA-DADOS-RST.
               05  GFCTNA-QTDE-OCOR                 PIC  9(007).
               05  GFCTNA-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNA-FUNC-BDSCO                PIC  X(007).
           03  GFCTNA-ERRO                          PIC  9(001).
           03  GFCTNA-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNA-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNA-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTNA-FIM                           PIC  X(001).
           03  GFCTNA-DADOS-CHAVE.
               05  GFCTNA-AGRUPAMENTO               PIC  9(003).
               05  GFCTNA-COD-TARIFA                PIC  9(005).
           03  GFCTNA-DADOS.
ST2506*        05  GFCTNA-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTNA-FILIAL                    PIC  9(005).
ST2506         05  GFCTNA-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTNA-FILIAL                    PIC  X(004).
               05  GFCTNA-CONTROLE                  PIC  X(002).
               05  GFCTNA-SEGMENTO                  PIC  9(003).
               05  GFCTNA-AGENCIA                   PIC  9(005).
               05  GFCTNA-POSTO                     PIC  9(005).
               05  GFCTNA-MUNIC                     PIC  9(007).
               05  GFCTNA-UF                        PIC  X(002).
               05  GFCTNA-TIMESTAMP                 PIC  X(026).
               05  GFCTNA-TODAS-AG                  PIC  X(001).
      * S = SIM
      * N = NAO
               05  GFCTNA-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTNA-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTNA-AGENDAMENTO               PIC  X(001).
      * S = SIM
      * N = NAO (NESTE CASO DIA AGENDAMENTO = 0)
               05  GFCTNA-DIA-AGEND                 PIC  9(002).
               05  GFCTNA-DIAS-RETENCAO             PIC  9(002).
               05  GFCTNA-IND-VALOR                 PIC  9(001).
      * 1 = SEM VALOR NEGOCIADO
      * 2 = VALOR NEGOCIADO
      * 3 = PERCENTUAL DE DESCONTO
               05  GFCTNA-VR-NEGOCIADO              PIC  9(009)V99.
               05  GFCTNA-PERC-DESCTO               PIC  9(003)V9(04).
               05  GFCTNA-FRANQUIA                  PIC  9(003).
               05  GFCTNA-PERIODO                   PIC  9(001).
      * 1 = DIARIO
      * 2 = SEMANAL
      * 3 = QUINZENAL
      * 4 = MENSAL
               05  GFCTNA-CTIPO-OPER-MOTVO          PIC  9(003).
               05  GFCTNA-CMOTVO-JUSTF              PIC  9(002).
               05  GFCTNA-RJUSTF-MANUT-SERVC        PIC  X(255).
               05  GFCTNA-SITUACAO-FLEX             PIC  9(002).
      * 01 = AGUARDANDO PARECER
      * 02 = DEFERIDA
      * 03 = INDEFERIDA
      * 04 = INDEFERIDA POR DECURSO DE PRAZO
      * 05 = DEFERIDA CANCELADA (FUTURA)
      * 06 = PEDIDO CANCELADO
               05  GFCTNA-DIA-SEMANA                PIC  9(001).
      * 1 = DOMINGO
      * 2 = SEGUNDA
      * 3 = TERCA
      * 4 = QUARTA
      * 5 = QUINTA
      * 6 = SEXTA
      * 7 = SABADO
080508         05  GFCTNA-PERIODO-COBR              PIC  9(001).
080508* 1 = DIARIO
080508* 2 = SEMANAL
080508* 3 = QUINZENAL
080508* 4 = MENSAL
080508         05  GFCTNA-INIC-PER-COBR             PIC  9(001).
080508* 1 - DOMINGO
080508* 2 - SEGUNDA-FEIRA
080508* 3 - TERCA-FEIRA
080508* 4 - QUARTA-FEIRA
080508* 5 - QUINTA-FEIRA
080508* 6 - SEXTA-FEIRA
080508* 7 - SABADO
               05  GFCTNA-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNA-SUB-GRUPO-CTBIL           PIC  9(003).
080508     03  FILLER                               PIC  X(483).
