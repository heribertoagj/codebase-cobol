      ******************************************************************
      * NOME DA INC - I#GFCTOB                                         *
      * DESCRICAO   - COMBO DE PACOTES - TABELA GFCTB048               *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 29/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTOB-ENTRADA.
           03  GFCTOB-LL                            PIC S9(004) COMP.
           03  GFCTOB-ZZ                            PIC S9(004) COMP.
           03  GFCTOB-TRANSACAO                     PIC  X(009).
           03  GFCTOB-FUNCAO                        PIC  X(008).
           03  GFCTOB-DADOS-RST.
               05  GFCTOB-FILTRO                    PIC  9(005).
               05  GFCTOB-QTDE-OCOR                 PIC  9(003).
               05  GFCTOB-QTDE-TOT-REG              PIC  9(006).
               05  GFCTOB-FUNC-BDSCO                PIC  X(007).
               05  GFCTOB-PONTEIRO.
                   07 GFCTOB-PONTEIRO-PACOTE        PIC  9(005).
               05  GFCTOB-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(052).
