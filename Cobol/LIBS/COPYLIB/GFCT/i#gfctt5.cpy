      *****************************************************************
      * NOME DA INC - I#GFCTT5                                        *
      * DESCRICAO   - FLEXIBILIZACAO AGRUPAMENTO - SAIDA   -  GFCT1705*
      *               BATCH DEFERIMENTO            ENTRADA -  GFCT1715*
      * TAMANHO     - 100                                             *
      * GFCTT5-LL   - 100                                             *
      * DATA        - 01.06.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTT5-REGISTRO.
           03  GFCTT5-DADOS-CHAVE.
               05  GFCTT5-CSERVC-TARIF                   PIC  9(005).
               05  GFCTT5-CAGPTO-CTA                     PIC  9(003).
               05  GFCTT5-HIDTFD-FLEXZ-AGPTO             PIC  X(026).
           03  GFCTT5-DADOS.
               05  GFCTT5-DATA-INICIO                    PIC  X(010).
               05  GFCTT5-DATA-FIM                       PIC  X(010).
               05  GFCTT5-DATA-FIM-ATU                   PIC  X(010).
           03  FILLER                                    PIC  X(036).
