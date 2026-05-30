      *****************************************************************
      * NOME DA INC - I#GFCTOV                                        *
      * DESCRICAO   - FLEXIBILIZACAO  AGRUPAMENTO - CANCEL PEDIDOS    *
      *               COMUNICACAO WEB X IMS   - ENTRADA - GFCT0886    *
      * TAMANHO     - 200                                             *
      * GFCTOV-LL   - 200                                             *
      * DATA        - 28.12.2005                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTOV-ENTRADA.
           03  GFCTOV-LL                            PIC S9(004) COMP.
           03  GFCTOV-ZZ                            PIC S9(004) COMP.
           03  GFCTOV-TRANSACAO                     PIC  X(009).
           03  GFCTOV-FUNCAO                        PIC  X(008).
           03  GFCTOV-DADOS-RST.
               05  GFCTOV-QTDE-OCOR                 PIC  9(007).
               05  GFCTOV-QTDE-TOT-REG              PIC  9(009).
               05  GFCTOV-FUNC-BDSCO                PIC  X(007).
               05  GFCTOV-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(075).
           03  GFCTOV-DADOS.
               05  GFCTOV-AGRUPMTO                  PIC  9(003).
               05  GFCTOV-TARIFA                    PIC  9(005).
               05  GFCTOV-TIMESTAMP                 PIC  X(026).
               05  GFCTOV-SEQ                       PIC  9(002).
ST2506*        05  GFCTOV-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTOV-FILIAL                    PIC  9(005).
ST2506         05  GFCTOV-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTOV-FILIAL                    PIC  X(004).
               05  GFCTOV-CONTROLE                  PIC  X(002).
               05  GFCTOV-AGENCIA                   PIC  9(005).
               05  GFCTOV-POSTO                     PIC  9(005).
               05  GFCTOV-SEGMENTO                  PIC  9(003).
               05  GFCTOV-MUNICIPIO                 PIC  9(007).
               05  GFCTOV-UF                        PIC  X(002).
               05  GFCTOV-GRUPO-CTBIL               PIC  9(003).
               05  GFCTOV-SUB-GRUPO-CTBIL           PIC  9(003).
