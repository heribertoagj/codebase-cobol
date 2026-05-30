      *****************************************************************
      * NOME DA INC - I#GFCTNT                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO  - ENCERRAMENTO PEDIDOS  *
      *               COMUNICACAO WEB X IMS   - ENTRADA - GFCT0887    *
      * TAMANHO     - 300                                             *
      * GFCTNT-LL   - 300                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTNT-ENTRADA.
           03  GFCTNT-LL                            PIC S9(004) COMP.
           03  GFCTNT-ZZ                            PIC S9(004) COMP.
           03  GFCTNT-TRANSACAO                     PIC  X(009).
           03  GFCTNT-FUNCAO                        PIC  X(008).
           03  GFCTNT-DADOS-RST.
               05  GFCTNT-QTDE-OCOR                 PIC  9(007).
               05  GFCTNT-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNT-FUNC-BDSCO                PIC  X(007).
               05  GFCTNT-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(157).
           03  GFCTNT-DADOS.
               05  GFCTNT-TIMESTAMP                 PIC  X(026).
               05  GFCTNT-AGRUPMTO                  PIC  9(003).
ST2506*        05  GFCTNT-CGC-CPF                   PIC  9(009).
ST2506*        05  GFCTNT-FILIAL                    PIC  9(005).
ST2506         05  GFCTNT-CGC-CPF                   PIC  X(009).
ST2506         05  GFCTNT-FILIAL                    PIC  X(004).
               05  GFCTNT-CONTROLE                  PIC  X(002).
               05  GFCTNT-AGENCIA                   PIC  9(005).
               05  GFCTNT-POSTO                     PIC  9(005).
               05  GFCTNT-SEGMENTO                  PIC  9(003).
               05  GFCTNT-MUNICIPIO                 PIC  9(007).
               05  GFCTNT-UF                        PIC  X(002).
               05  GFCTNT-TARIFA                    PIC  9(005).
               05  GFCTNT-DATA-INIC                 PIC  X(010).
               05  GFCTNT-DATA-FIM                  PIC  X(010).
               05  GFCTNT-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNT-SUB-GRUPO-CTBIL           PIC  9(003).
