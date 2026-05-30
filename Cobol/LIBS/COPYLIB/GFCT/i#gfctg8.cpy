      ******************************************************************
      * NOME DA INC - I#GFCTG8                                         *
      * DESCRICAO   - COMBO SEGMENTOS                                  *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 14/09/2005                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 4152-4352       *
      ******************************************************************
      *
       01  GFCTG8-SAIDA.
           03  GFCTG8-LL                            PIC S9(004) COMP.
           03  GFCTG8-ZZ                            PIC S9(004) COMP.
           03  GFCTG8-TRANSACAO                     PIC  X(009).
           03  GFCTG8-FUNCAO                        PIC  X(008).
           03  GFCTG8-FILTRO                        PIC  9(003).
           03  GFCTG8-QTDE-OCOR                     PIC  9(003).
           03  GFCTG8-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTG8-FUNC-BDSCO                    PIC  X(007).
           03  GFCTG8-TERMINAL                      PIC  X(008).
           03  GFCTG8-PONTEIRO                      PIC  9(003).
           03  GFCTG8-ERRO                          PIC  9(001).
           03  GFCTG8-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTG8-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTG8-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTG8-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(020).
           03  GFCTG8-DADOS.
               05  FILLER                           OCCURS 16.
                   07  GFCTG8-COD-SEGMENTO          PIC  9(003).
                   07  GFCTG8-DESC-SEGMENTO         PIC  X(030).
                   07  GFCTG8-TIPO-PESSOA           PIC  X(010).
                   07  GFCTG8-SIT-SEGMENTO          PIC  X(010).
