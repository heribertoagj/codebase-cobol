      ******************************************************************
      * NOME DA INC - I#GFCTWG                                         *
      * DESCRICAO   - LISTA LIBER. SUSPENSAO COBRANCA TARIFAS AGRUP.   *
      *               COMUNICACAO IMS X WEB - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 03/2007                                          *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352    *
      ******************************************************************
       01  GFCTWG-ENTRADA.
           03  GFCTWG-LL                            PIC S9(004) COMP.
           03  GFCTWG-ZZ                            PIC S9(004) COMP.
           03  GFCTWG-TRANSACAO                     PIC  X(009).
           03  GFCTWG-FUNCAO                        PIC  X(008).
           03  GFCTWG-DADOS-RST.
               05  GFCTWG-FILTRO.
                   07  GFCTWG-FL-IND-LIBERACAO      PIC  X(001).
      *---             'A' - AGENDADA
      *---             'E' - EFETIVADA
      *---             'T' - TODAS
                   07  GFCTWG-FL-COD-AGRUPAMENTO    PIC  9(003).
                   07  GFCTWG-FL-COD-TARIFA         PIC  9(005).
               05  GFCTWG-QTDE-OCOR                 PIC  9(003).
               05  GFCTWG-QTDE-TOT-REG              PIC  9(006).
               05  GFCTWG-FUNC-BDSCO                PIC  X(007).
               05  GFCTWG-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(053).
