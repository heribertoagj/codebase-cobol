      ******************************************************************
      * NOME DA INC - I#GFCTG7                                         *
      * DESCRICAO   - COMBO SEGMENTOS                                  *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 14/09/2005                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTG7-ENTRADA.
           03  GFCTG7-LL                            PIC S9(004) COMP.
           03  GFCTG7-ZZ                            PIC S9(004) COMP.
           03  GFCTG7-TRANSACAO                     PIC  X(009).
           03  GFCTG7-FUNCAO                        PIC  X(008).
           03  GFCTG7-DADOS-RST.
               05  GFCTG7-FILTRO                    PIC  9(003).
               05  GFCTG7-QTDE-OCOR                 PIC  9(003).
               05  GFCTG7-QTDE-TOT-REG              PIC  9(006).
               05  GFCTG7-FUNC-BDSCO                PIC  X(007).
               05  GFCTG7-TERMINAL                  PIC  X(008).
               05  GFCTG7-PONTEIRO                  PIC  9(003).
               05  GFCTG7-FIM                       PIC  X(001).
               05  GFCTG7-SIT-SEGMENTO              PIC  9(001).
      *            0 - SEGMENTOS ATIVOS
      *            1 - SEGMENTOS ENCERRADOS
      *            2 - ATIVOS E ENCERRADOS
               05  FILLER                           PIC  X(047).
