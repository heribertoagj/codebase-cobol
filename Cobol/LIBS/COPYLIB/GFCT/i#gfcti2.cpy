***************************** Top of Data ******************************
      ******************************************************************
      * NOME DA INC - I#GFCTI2                                         *
      * DESCRICAO   - COMBO DE CONSULTA DE ASSOCIACAO DE SEGMENTOS     *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 23/11/2005                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 4152-4352       *
      ******************************************************************
      *
       01  GFCTI2-SAIDA.
           03  GFCTI2-LL                            PIC S9(004) COMP.
           03  GFCTI2-ZZ                            PIC S9(004) COMP.
           03  GFCTI2-TRANSACAO                     PIC  X(009).
           03  GFCTI2-FUNCAO                        PIC  X(008).
           03  GFCTI2-FILTRO                        PIC  9(003).
           03  GFCTI2-QTDE-OCOR                     PIC  9(003).
           03  GFCTI2-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTI2-FUNC-BDSCO                    PIC  X(007).
           03  GFCTI2-TERMINAL                      PIC  X(008).
           03  GFCTI2-PONTEIRO                      PIC  9(003).
           03  GFCTI2-ERRO                          PIC  9(001).
           03  GFCTI2-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTI2-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTI2-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTI2-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(020).
           03  GFCTI2-DADOS.
               05  FILLER                           OCCURS 16.
                   07  GFCTI2-COD-SEGMENTO          PIC  9(003).
                   07  GFCTI2-DESC-SEGMENTO         PIC  X(030).
                   07  GFCTI2-TIPO-PESSOA           PIC  X(010).
                   07  GFCTI2-SIT-SEGMENTO          PIC  X(010).
