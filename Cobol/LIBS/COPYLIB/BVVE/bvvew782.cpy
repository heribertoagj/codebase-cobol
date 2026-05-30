      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW782                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3782             *
      * DATA      : 11/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 1537 BYTES                                         *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW782-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW782-E-DT-MOV-INI      :   DATA INICIAL DO MOVIMENTO       *
      * BVVEW782-E-DT-MOV-FIM      :   DATA FINAL DO MOVIMENTO         *
      * BVVEW782-E-ORIGEM.         :   ORIGEM                          *
      *    BVVEW782-E-ISPB-ORIGE   :       ISPB ORIGEM                 *
      * BVVEW782-E-DESTINO.        :   DESTINO                         *
      *    BVVEW782-E-ISPB-DSTNO   :       ISPB DESTINO                *
      * BVVEW782-E-NU-CTRL-EMIS    :   NUMERO DE CONTROLE EMISSOR      *
      * BVVEW782-E-SITU-MSGEM      :   SITUACAO DA MENSAGEM            *
      * BVVEW782-E-NCTRL-IF        :   NUMERO DE CONTROLE DA INSTI-    *
      *                                TUICAO FINANCEIRA               *
      * BVVEW782-E-NOME-ARQUIVO    :   NOME DO ARQUIVO                 *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW782-REGISTRO.
               10 BVVEW782-BLOCO-ENTRADA.
                   15 BVVEW782-E-CHAMADOR          PIC  X(008).
                   15 BVVEW782-E-DT-MOV-INI        PIC  X(010).
                   15 BVVEW782-E-DT-MOV-FIM        PIC  X(010).
                   15 BVVEW782-E-ORIGEM.
                       20 BVVEW782-E-ISPB-ORIGE    PIC  X(008).
                   15 BVVEW782-E-DESTINO.
                       20 BVVEW782-E-ISPB-DSTNO    PIC  X(008).
                   15 BVVEW782-E-NU-CTRL-EMIS      PIC  X(023).
                   15 BVVEW782-E-SITU-MSGEM        PIC  9(003).
                   15 BVVEW782-E-EIBTRMID          PIC  X(008).
                   15 BVVEW782-E-TP-CONSULTA       PIC  X(001).
                   15 BVVEW782-E-NOME-ARQUIVO      PIC  X(037).
                   15 BVVEW782-E-EVENTO            PIC  X(007).
               10 BVVEW782-BLOCO-RESTART.
                   15 BVVEW782-CMNTRC-REST-INI     PIC  X(023).
                   15 BVVEW782-CMNTRC-REST-FIM     PIC  X(023).
               10 BVVEW782-BLOCO-SAIDA OCCURS 12 TIMES.
                   15 BVVEW782-S-CMNTRC-MSGRA      PIC  X(023).
                   15 BVVEW782-S-NMNTRC-TRFGO-ARQ  PIC  9(018).
                   15 BVVEW782-S-NCTRL-IF          PIC  X(020).
                   15 BVVEW782-S-NOME-ARQUIVO      PIC  X(050).
               10 BVVEW782-BLOCO-RETORNO.
                   15 BVVEW782-COD-RET             PIC  9(002).
                   15 BVVEW782-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
