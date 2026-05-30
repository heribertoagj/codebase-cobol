      *****************************************************************
      * NOME DA INC - I#GFCTOX                                        *
      * DESCRICAO   - FLEXIBILIZACAO  AGRUPAMENTO - CANCEL PEDIDOS    *
      *               COMUNICACAO WEB X IMS - SAIDA - GFCT0886        *
      * TAMANHO     - 1010                                            *
      * GFCTOX-LL   - 1010                                            *
      * DATA        - 28.12.2005                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTOX-SAIDA.
           03  GFCTOX-LL                            PIC S9(004) COMP.
           03  GFCTOX-ZZ                            PIC S9(004) COMP.
           03  GFCTOX-TRANSACAO                     PIC  X(009).
           03  GFCTOX-FUNCAO                        PIC  X(008).
           03  GFCTOX-DADOS-RST.
               05  GFCTOX-QTDE-OCOR                 PIC  9(007).
               05  GFCTOX-QTDE-TOT-REG              PIC  9(009).
               05  GFCTOX-FUNC-BDSCO                PIC  X(007).
               05  GFCTOX-FIM                       PIC  X(001).
           03  GFCTOX-ERRO                          PIC  9(001).
           03  GFCTOX-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOX-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOX-DESC-MSG-ERRO                 PIC  X(079).
           03  FILLER                               PIC  X(688).
           03  GFCTOX-DADOS.
               05  GFCTOX-AGRUPMTO                  PIC  9(003).
ST2506*        05  GFCTOX-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTOX-FILIAL                    PIC  9(005).
ST2506         05  GFCTOX-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTOX-FILIAL                    PIC  X(004).
               05  GFCTOX-CONTROLE                  PIC  X(002).
               05  GFCTOX-AGENCIA                   PIC  9(005).
               05  GFCTOX-POSTO                     PIC  9(005).
               05  GFCTOX-SEGMENTO                  PIC  9(003).
               05  GFCTOX-MUNICIPIO                 PIC  9(007).
               05  GFCTOX-UF                        PIC  X(002).
               05  GFCTOX-TARIFA                    PIC  9(005).
               05  GFCTOX-TIMESTAMP                 PIC  X(026).
               05  GFCTOX-SEQUENCIA                 PIC  9(002).
               05  GFCTOX-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTOX-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTOX-DESCR-AGPTO               PIC  X(030).
               05  GFCTOX-NOME-TARIFA               PIC  X(030).
               05  GFCTOX-DIA-AGEND                 PIC  9(002).
               05  GFCTOX-FRANQUIA                  PIC  9(003).
               05  GFCTOX-PERIODO                   PIC  9(001).
               05  GFCTOX-VR-NEGOCIADO              PIC  9(009)V99.
               05  GFCTOX-PERC-DESCTO               PIC  9(003)V9(04).
               05  GFCTOX-DIAS-RETENCAO             PIC  9(002).
               05  GFCTOX-FLAG-ALT-VIG              PIC  X(001).
               05  GFCTOX-GRUPO-CTBIL               PIC  9(003).
               05  GFCTOX-SUB-GRUPO-CTBIL           PIC  9(003).
