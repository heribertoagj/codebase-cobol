      *****************************************************************
      * NOME DA INC - I#GFCTNU                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - ENCERRAMENTO           *
      *               COMUNICACAO WEB X IMS  - SAIDA - GFCT0887       *
      * TAMANHO     - 1010                                            *
      * GFCTNU-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNU-SAIDA.
           03  GFCTNU-LL                            PIC S9(004) COMP.
           03  GFCTNU-ZZ                            PIC S9(004) COMP.
           03  GFCTNU-TRANSACAO                     PIC  X(009).
           03  GFCTNU-FUNCAO                        PIC  X(008).
           03  GFCTNU-DADOS-RST.
               05  GFCTNU-QTDE-OCOR                 PIC  9(007).
               05  GFCTNU-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNU-FUNC-BDSCO                PIC  X(007).
               05  GFCTNU-FIM                       PIC  X(001).
           03  GFCTNU-ERRO                          PIC  9(001).
           03  GFCTNU-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNU-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNU-DESC-MSG-ERRO                 PIC  X(079).
           03  FILLER                               PIC  X(691).
           03  GFCTNU-DADOS.
               05  GFCTNU-TIMESTAMP                 PIC  X(026).
               05  GFCTNU-AGRUPMTO                  PIC  9(003).
ST2506*        05  GFCTNU-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTNU-FILIAL                    PIC  9(005).
ST2506         05  GFCTNU-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTNU-FILIAL                    PIC  X(004).
               05  GFCTNU-CONTROLE                  PIC  X(002).
               05  GFCTNU-AGENCIA                   PIC  9(005).
               05  GFCTNU-POSTO                     PIC  9(005).
               05  GFCTNU-SEGMENTO                  PIC  9(003).
               05  GFCTNU-MUNICIPIO                 PIC  9(007).
               05  GFCTNU-UF                        PIC  X(002).
               05  GFCTNU-TARIFA                    PIC  9(005).
               05  GFCTNU-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTNU-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTNU-DESCR-AGPTO               PIC  X(030).
               05  GFCTNU-NOME-TARIFA               PIC  X(030).
               05  GFCTNU-DIA-AGEND                 PIC  9(002).
               05  GFCTNU-FRANQUIA                  PIC  9(003).
               05  GFCTNU-PERIODO                   PIC  9(001).
               05  GFCTNU-VR-NEGOCIADO              PIC  9(009)V99.
               05  GFCTNU-PERC-DESCTO               PIC  9(003)V9(04).
               05  GFCTNU-DIAS-RETENCAO             PIC  9(002).
               05  GFCTNU-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNU-SUB-GRUPO-CTBIL           PIC  9(003).

