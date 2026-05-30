***************************** Top of Data ******************************
      ******************************************************************
      * NOME DA INC - I#GFCTI1                                         *
      * DESCRICAO   - COMBO DE CONSULTA DE ASSOCIACAO DE SEGMENTOS     *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 23/11/2005                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTI1-ENTRADA.
           03  GFCTI1-LL                            PIC S9(004) COMP.
           03  GFCTI1-ZZ                            PIC S9(004) COMP.
           03  GFCTI1-TRANSACAO                     PIC  X(009).
           03  GFCTI1-FUNCAO                        PIC  X(008).
           03  GFCTI1-DADOS-RST.
               05  GFCTI1-FILTRO                    PIC  9(003).
               05  GFCTI1-QTDE-OCOR                 PIC  9(003).
               05  GFCTI1-QTDE-TOT-REG              PIC  9(006).
               05  GFCTI1-FUNC-BDSCO                PIC  X(007).
               05  GFCTI1-TERMINAL                  PIC  X(008).
               05  GFCTI1-PONTEIRO                  PIC  9(003).
               05  GFCTI1-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(048).
