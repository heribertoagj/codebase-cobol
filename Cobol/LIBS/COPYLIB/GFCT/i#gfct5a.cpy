      *****************************************************************
      * NOME DA INC - I#GFCT5A                                        *
      * DESCRICAO   - DADOS B SICOS - ENTRADA GRUPO DE LANCAMENTOS    *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 300                                             *
      * GFCT5A-LL   - 1010                                            *
      * DATA        - 02.02.2005                                      *
      * RESPONSAVEL - MARCELO  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCT5A-ENTRADA.
           03  GFCT5A-LL                            PIC S9(004) COMP.
           03  GFCT5A-ZZ                            PIC S9(004) COMP.
           03  GFCT5A-TRANSACAO                     PIC  X(009).
           03  GFCT5A-FUNCAO                        PIC  X(008).
           03  GFCT5A-DADOS-RST.
               05  GFCT5A-FILTRO                    PIC  9(005).
               05  GFCT5A-QTDE-OCOR                 PIC  9(005).
               05  GFCT5A-QTDE-TOT-REG              PIC  9(006).
               05  GFCT5A-FUNC-BDSCO                PIC  X(007).
               05  GFCT5A-TERMINAL                  PIC  X(008).
               05  GFCT5A-PONTEIRO                  PIC  9(005).
               05  GFCT5A-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(067).
           03  GFCT5A-DADOS.
               05  FILLER                           OCCURS 5.
                   07  GFCT5A-COD-GRP               PIC  9(005).
                   07  GFCT5A-DESC-GRP              PIC  X(030).
