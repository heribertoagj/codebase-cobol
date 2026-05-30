      *****************************************************************
      * NOME DA INC - GFCTWAK1                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               COMBO DE LOTE - DEFERIMENTO FLEXIBILIZACAO      *
      *               POR AG/CONTA                                    *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                 GFCT5213    *
      * DATA        - 25.01.2010                                      *
      * RESPONSAVEL - CIBELE          - PROCWORK                      *
      *****************************************************************
      *
       01  WAK1-ENTRADA.
           03 WAK1-LL                            PIC S9(004) COMP.
           03  WAK1-ZZ                           PIC S9(004) COMP.
           03  WAK1-TRANSACAO                    PIC  X(009).
           03  WAK1-FUNCAO                       PIC  X(008).
           03  WAK1-DADOS-RST.
               05  WAK1-FILTRO.
                   07 WAK1-FLT-CJUNC-DEPDC       PIC  9(005).
                   07 WAK1-FLT-CCTA-CLI          PIC  9(013).
                   07 WAK1-FLT-CONJ-SERV         PIC  9(003).
               05  WAK1-QTDE-OCOR                PIC  9(003).
               05  WAK1-QTDE-TOT-REG             PIC  9(006).
               05  WAK1-FUNC-BDSCO               PIC  X(007).
               05  WAK1-PONTEIRO.
                   07 WAK1-PONT-DESPREZ          PIC  9(005).
               05  WAK1-FIM                      PIC  X(001).
           03  FILLER                            PIC  X(036).
