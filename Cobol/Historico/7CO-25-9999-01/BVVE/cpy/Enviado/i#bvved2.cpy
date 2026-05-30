      *----------------------------------------------------------------*
      *  I#BVVED2 - ARQUIVO DE MENSAGENS DE MANUTENCAO DE DOMICILIO    *
      *  BANCARIO - TRVPMSG                                            *
      *  DSNAME: CB.BACC.VON.TRAVPMSG                                  *
      *----------------------------------------------------------------*

       01  BVVED2-TRVPMSG.
           03 BVVED2-CHAVE.
              05 BVVED2-NUM-CTRL-IF    PIC X(020).
              05 BVVED2-NUOP           PIC X(023).
              05 BVVED2-FILLER-CHV     PIC X(057).
7C2511     03 BVVED2-CNPJ              PIC X(009).
7C2511*     03 BVVED2-CNPJ              PIC 9(009).
7C2511     03 BVVED2-FILIAL            PIC X(004).
7C2511*     03 BVVED2-FILIAL            PIC 9(004).
           03 BVVED2-COD-MSG           PIC X(009).
           03 BVVED2-ERRO-GEN          PIC X(008).
           03 BVVED2-ISPB-EMISSOR      PIC X(008).
           03 BVVED2-ISPB-DESTIN       PIC X(008).
           03 BVVED2-NUM-MQ            PIC X(048).
           03 BVVED2-NU-OP-OR          PIC X(023).
           03 BVVED2-NUM-PROT-PSTA-OR  PIC X(008).
           03 BVVED2-HIST              PIC X(200).
           03 BVVED2-DT-HR-PART        PIC X(020).
           03 BVVED2-STATUS            PIC X(002).
           03 BVVED2-AREA-ERRO.
               05 BVVED2-ERRO              PIC 9(002).
016            05 BVVED2-COMPLETION-CODE   PIC 9(008).
019            05 BVVED2-REASON-CODE       PIC 9(008).
019            05 BVVED2-EIBRESP           PIC 9(008).
           03 FILLER                       PIC X(77).
      *    STATUS
      *    01 - ENVIADO AO TMNF (BVVE4071)
      *    02 - RECEBIDO PELO TMNF (BVVE4072)
      *    03 - RECEBIDO DA CIP (BVVE4073)

