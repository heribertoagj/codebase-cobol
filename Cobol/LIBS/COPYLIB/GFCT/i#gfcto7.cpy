      ******************************************************************
      * NOME DA INC - I#GFCTO7                                         *
      * DESCRICAO   - COMBO DE PACOTE - TABELA GFCTB048                *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 24/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTO7-ENTRADA.
           03  GFCTO7-LL                            PIC S9(004) COMP.
           03  GFCTO7-ZZ                            PIC S9(004) COMP.
           03  GFCTO7-TRANSACAO                     PIC  X(009).
           03  GFCTO7-FUNCAO                        PIC  X(008).
           03  GFCTO7-DADOS-RST.
               05  GFCTO7-FILTRO.
                   07  GFCTO7-COD-DEPDC             PIC  9(005).
                   07  GFCTO7-CCTA-CLI              PIC  9(013).
               05  GFCTO7-QTDE-OCOR                 PIC  9(003).
               05  GFCTO7-QTDE-TOT-REG              PIC  9(006).
               05  GFCTO7-FUNC-BDSCO                PIC  X(007).
               05  GFCTO7-PONTEIRO.
                   07 GFCTO7-PONTEIRO-PACOTE        PIC  9(005).
                   07 GFCTO7-PONTEIRO-DEPDC         PIC  9(005).
                   07 GFCTO7-PONTEIRO-CCTA          PIC  9(013).
               05  GFCTO7-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(021).
