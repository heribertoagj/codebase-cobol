      *****************************************************************
      * NOME DA INC - I#GFCT1V                                        *
      * DESCRICAO   - OBTER DESCRICAO DE DEPENDENCIA                  *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 680                                             *
      * GFCT1V-LL   - 680                                             *
      * DATA        - 20.11.2004                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCT1V-ENTRADA.
           03  GFCT1V-LL                            PIC S9(004) COMP.
           03  GFCT1V-ZZ                            PIC S9(004) COMP.
           03  GFCT1V-TRANSACAO                     PIC  X(009).
           03  GFCT1V-FUNCAO                        PIC  X(008).
           03  GFCT1V-QTDE-OCOR                     PIC  9(003).
           03  GFCT1V-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT1V-FUNC-BDSCO                    PIC  X(007).
           03  GFCT1V-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(617).
           03  GFCT1V-DADOS.
               05  FILLER                           OCCURS 5.
                   07  GFCT1V-COD-DEPDC             PIC  9(005).
