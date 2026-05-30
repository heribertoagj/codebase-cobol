      ******************************************************************
      * NOME DA INC - I#GFCTOD                                         *
      * DESCRICAO   - COMBO DE TIPO DE AGRUPAMENTO                     *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 29/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTOD-ENTRADA.
           03  GFCTOD-LL                            PIC S9(004) COMP.
           03  GFCTOD-ZZ                            PIC S9(004) COMP.
           03  GFCTOD-TRANSACAO                     PIC  X(009).
           03  GFCTOD-FUNCAO                        PIC  X(008).
           03  GFCTOD-DADOS-RST.
               05  GFCTOD-FILTRO                    PIC  9(005).
               05  GFCTOD-QTDE-OCOR                 PIC  9(003).
               05  GFCTOD-QTDE-TOT-REG              PIC  9(006).
               05  GFCTOD-FUNC-BDSCO                PIC  X(007).
               05  GFCTOD-PONTEIRO.
                   07 GFCTOD-PONTEIRO-AGRUP         PIC  9(003).
               05  GFCTOD-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(054).
