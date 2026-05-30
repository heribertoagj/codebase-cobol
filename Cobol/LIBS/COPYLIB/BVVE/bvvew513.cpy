      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW513                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3513             *
      * DATA      : 09/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00534 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW513-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW513-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW513-E-DT-MOV-INI      :   DATA INICIAL DO MOVIMENTO       *
      * BVVEW513-E-DT-MOV-FIM      :   DATA FINAL DO MOVIMENTO         *
      * BVVEW513-E-HR-MOV-INI      :   HORA INICIAL DO MOVIMENTO       *
      * BVVEW513-E-HR-MOV-FIM      :   HORA FINAL DO MOVIMENTO         *
      * BVVEW513-E-ORIGEM.         :   ORIGEM                          *
      *    BVVEW513-E-CBCO-ORIGE   :       BANCO ORIGEM                *
      *    BVVEW513-E-ISPB-ORIGE   :       ISPB ORIGEM                 *
      * BVVEW513-E-DESTINO.        :   DESTINO                         *
      *    BVVEW513-E-CBCO-DSTNO   :       BANCO DESTINO               *
      *    BVVEW513-E-ISPB-DSTNO   :       ISPB DESTINO                *
      * BVVEW513-E-EVENTO.         :   EVENTO                          *
      *    BVVEW513-E-CSIST-EXTER  :       SISTEMA EXTERNO             *
      *    BVVEW513-E-CEVNTO-MSG   :       EVENTO                      *
      *    BVVEW513-E-CFLUXO-MSG   :       FLUXO                       *
      * BVVEW513-E-NU-CTRL-EMIS    :   NUMERO DE CONTROLE EMISSOR      *
      * BVVEW513-E-NU-CTRL-IF      :   NUMERO DE CONTROLE DA IF        *
MB0208* BVVEW513-E-CCONTR          :   CONTRATO BRADESCO               *
      * BVVEW513-E-SITU-MSGEM      :   SITUACAO DA MENSAGEM            *
      * BVVEW513-E-FLUXO-MSGEM     :   FLUXO DA MENSAGEM               *
      *                                    E - ENVIADA                 *
      *                                    R - RECEBIDA                *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW513-REGISTRO.
               10 BVVEW513-BLOCO-ENTRADA.
                   15 BVVEW513-E-CHAMADOR          PIC  X(008).
                   15 BVVEW513-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW513-E-DT-MOV-INI        PIC  X(010).
                   15 BVVEW513-E-DT-MOV-FIM        PIC  X(010).
                   15 BVVEW513-E-HR-MOV-INI        PIC  X(008).
                   15 BVVEW513-E-HR-MOV-FIM        PIC  X(008).
                   15 BVVEW513-E-ORIGEM.
                       20 BVVEW513-E-CBCO-ORIGE    PIC  9(003).
                       20 BVVEW513-E-ISPB-ORIGE    PIC  X(008).
                   15 BVVEW513-E-DESTINO.
                       20 BVVEW513-E-CBCO-DSTNO    PIC  9(003).
                       20 BVVEW513-E-ISPB-DSTNO    PIC  X(008).
                   15 BVVEW513-E-EVENTO.
                       20 BVVEW513-E-CSIST-EXTER   PIC  X(003).
MB1001                 20 BVVEW513-E-CEVNTO-MSG    PIC  X(004).
                       20 BVVEW513-E-CFLUXO-MSG    PIC  X(002).
                   15 BVVEW513-E-NU-CTRL-EMIS      PIC  X(023).
                   15 BVVEW513-E-NU-CTRL-IF        PIC  X(020).
MB0208             15 BVVEW513-E-CCONTR            PIC  X(020).
                   15 BVVEW513-E-SITU-MSGEM        PIC  9(003).
                   15 BVVEW513-E-FLUXO-MSGEM       PIC  X(001).
                   15 BVVEW513-E-EIBTRMID          PIC  X(008).
               10 BVVEW513-BLOCO-RESTART.
                   15 BVVEW513-CMNTRC-REST-INI     PIC  X(023).
                   15 BVVEW513-CMNTRC-REST-FIM     PIC  X(023).
               10 BVVEW513-BLOCO-SAIDA.
                   15 BVVEW513-S-CMNTRC-MSGRA      PIC  X(023)
                                                   OCCURS 12 TIMES.
               10 BVVEW513-BLOCO-RETORNO.
                   15 BVVEW513-COD-RET             PIC  9(002).
                   15 BVVEW513-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
