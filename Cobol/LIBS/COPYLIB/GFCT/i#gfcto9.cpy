      ******************************************************************
      * NOME DA INC - I#GFCTO9                                         *
      * DESCRICAO   - COMBO DE TIPO DE AGRUPAMENTO                     *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 24/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTO9-ENTRADA.
           03  GFCTO9-LL                            PIC S9(004) COMP.
           03  GFCTO9-ZZ                            PIC S9(004) COMP.
           03  GFCTO9-TRANSACAO                     PIC  X(009).
           03  GFCTO9-FUNCAO                        PIC  X(008).
           03  GFCTO9-DADOS-RST.
               05  GFCTO9-FILTRO.
                   07 GFCTO9-COD-DEPDC              PIC  9(005).
                   07 GFCTO9-CCTA-CLI               PIC  9(013).
                   07 GFCTO9-CSERVC-TARIF           PIC  9(005).
               05  GFCTO9-QTDE-OCOR                 PIC  9(003).
               05  GFCTO9-QTDE-TOT-REG              PIC  9(006).
               05  GFCTO9-FUNC-BDSCO                PIC  X(007).
               05  GFCTO9-PONTEIRO.
                   07 GFCTO9-PONTEIRO-DEPDC         PIC  9(005).
                   07 GFCTO9-PONTEIRO-CCTA          PIC  9(013).
                   07 GFCTO9-PONTEIRO-TARIF         PIC  9(005).
                   07 GFCTO9-PONTEIRO-AGRUP         PIC  9(003).
               05  GFCTO9-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(013).
