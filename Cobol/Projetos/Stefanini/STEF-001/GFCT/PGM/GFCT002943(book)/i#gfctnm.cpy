      *****************************************************************
      * NOME DA INC - I#GFCTNM                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO -  CONSULTA DETALHADA    *
      *               COMUNICACAO WEB X IMS - SAIDA - GFCT0884        *
      * TAMANHO     - 1010                                            *
      * GFCTNM-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNM-SAIDA.
           03  GFCTNM-LL                            PIC S9(004) COMP.
           03  GFCTNM-ZZ                            PIC S9(004) COMP.
           03  GFCTNM-TRANSACAO                     PIC  X(009).
           03  GFCTNM-FUNCAO                        PIC  X(008).
           03  GFCTNM-QTDE-OCOR                     PIC  9(007).
           03  GFCTNM-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTNM-FUNC-BDSCO                    PIC  X(007).
           03  GFCTNM-ERRO                          PIC  9(001).
           03  GFCTNM-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNM-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNM-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTNM-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(006).
           03  GFCTNM-DADOS-VIG-HIST.
               05  GFCTNM-AGRUPMTO                  PIC  9(003).
ST2506*        05  GFCTNM-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTNM-FILIAL                    PIC  9(005).
ST2506         05  GFCTNM-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTNM-FILIAL                    PIC  X(004).
               05  GFCTNM-CONTROLE                  PIC  X(002).
               05  GFCTNM-AGENCIA                   PIC  9(005).
               05  GFCTNM-POSTO                     PIC  9(005).
               05  GFCTNM-SEGMENTO                  PIC  9(003).
               05  GFCTNM-MUNICIPIO                 PIC  9(007).
               05  GFCTNM-UF                        PIC  X(002).
               05  GFCTNM-DESCR-AGPTO               PIC  X(030).
               05  GFCTNM-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNM-SUB-GRUPO-CTBIL           PIC  9(003).
               05  GFCTNM-TARIFA                    PIC  9(005).
               05  GFCTNM-TIMESTAMP                 PIC  X(026).
               05  GFCTNM-SEQUENCIA                 PIC  9(002).
               05  GFCTNM-NOME-TARIFA               PIC  X(030).
               05  GFCTNM-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTNM-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTNM-DIA-AGEND                 PIC  9(002).
               05  GFCTNM-FRANQUIA                  PIC  9(003).
               05  GFCTNM-PERIODO                   PIC  9(001).
               05  GFCTNM-DIA-SEMANA                PIC  9(001).

      * 1 = DOMINGO
      * 2 = SEGUNDA
      * 3 = TERCA
      * 4 = QUARTA
      * 5 = QUINTA
      * 6 = SEXTA
      * 7 = SABADO
               05  GFCTNM-VR-NEGOCIADO              PIC  9(009)V99.
               05  GFCTNM-PERC-DESCTO               PIC  9(003)V9(04).
               05  GFCTNM-DIAS-RETENCAO             PIC  9(002).
               05  GFCTNM-FLAG-ALT-VIG              PIC  X(001).
080508         05  GFCTNM-CPER-COBR-TARIF           PIC  9(001).
080508* 1 = DIARIO
080508* 2 = SEMANAL
080508* 3 = QUINZENAL
080508* 4 = MENSAL
080508         05  GFCTNM-CINIC-PER-COBR            PIC  9(001).
080508* 1 - DOMINGO
080508* 2 - SEGUNDA-FEIRA
080508* 3 - TERCA-FEIRA
080508* 4 - QUARTA-FEIRA
080508* 5 - QUINTA-FEIRA
080508* 6 - SEXTA-FEIRA
080508* 7 - SABADO
           03  GFCTNM-DADOS-HISTORICO.
               05  GFCTNM-CTIPO-OPER-MOTVO          PIC  9(003).
               05  GFCTNM-NOME-OPER-MOTVO           PIC  X(030).
               05  GFCTNM-CMOTVO-JUSTF              PIC  9(002).
               05  GFCTNM-NOME-MOTVO                PIC  X(030).
               05  GFCTNM-DESCR-SITUACAO            PIC  X(020).
               05  GFCTNM-SOLICITACAO.
250808*            07  GFCTNM-RJUSTF-SOLIC          PIC  X(255).
250808             07  GFCTNM-RJUSTF-SOLIC          PIC  X(215).
                   07  GFCTNM-JUNCAO-SOLIC          PIC  9(005).
                   07  GFCTNM-NOME-JCAO-SOL         PIC  X(030).
                   07  GFCTNM-DATA-SOLIC            PIC  X(010).
250808             07  GFCTNM-FUNC-SOLIC            PIC  X(007).
250808             07  GFCTNM-NOME-FUNC-SOLIC       PIC  X(030).
               05  GFCTNM-PARECER.
250808*            07  GFCTNM-RJUSTF-PAREC          PIC  X(255).
250808             07  GFCTNM-RJUSTF-PAREC          PIC  X(214).
                   07  GFCTNM-JUNCAO-PAREC          PIC  9(005).
                   07  GFCTNM-NOME-JCAO-PAREC       PIC  X(030).
                   07  GFCTNM-DATA-PAREC            PIC  X(010).
250808             07  GFCTNM-FUNC-PAREC            PIC  X(007).
250808             07  GFCTNM-NOME-FUNC-PAREC       PIC  X(030).
               05  GFCTNM-TIPO-CONSULTA             PIC  X(001).
      * V = PEDIDOS VIGENTES
      * H = HISTORICO
      * R = RENOVACAO
      * E = ENCERRAMENTO
      * C = CANCELAMENTO
