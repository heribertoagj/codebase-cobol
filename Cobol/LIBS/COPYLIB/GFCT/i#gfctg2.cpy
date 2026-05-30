***************************** Top of Data ******************************
      *****************************************************************
      * NOME DA INC - I#GFCTG2                                        *
      * DESCRICAO   - MODULO - OBTER MENSAGEM                         *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 10.09.2005                                      *
      * RESPONSAVEL - MAURICIO - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTG2-ENTRADA.
           03  GFCTG2-LL                            PIC S9(004) COMP.
           03  GFCTG2-ZZ                            PIC S9(004) COMP.
           03  GFCTG2-TRANSACAO                     PIC  X(009).
           03  GFCTG2-FUNCAO                        PIC  X(008).
           03  GFCTG2-FUNC-BDSCO                    PIC  X(007).
           03  GFCTG2-TIPO-PROC                     PIC  X(001).
           03  GFCTG2-COD-MSG                       PIC  X(004).
           03  GFCTG2-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(066).
