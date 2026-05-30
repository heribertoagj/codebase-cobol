      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW264                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3264             *
      * DATA      : 04/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01078 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW264-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW264-E-CMNTR-MSGRA     :   CHAVE DA MENSAGEM               *
      * BVVEW264-E-NETAPA-MNTRC    :   CHAVE DA MENSAGEM               *
      * BVVEW264-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      * BVVEW264-E-NU-CTRL-EMS     :   NUMERO DE CONTROLE EMISSOR      *
      * BVVEW264-E-EVNTO           :   EVENTO(CODIGO DA MENSAGEM       *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW264-REGISTRO.
               10 BVVEW264-BLOCO-ENTRADA.
                   15 BVVEW264-E-CHAMADOR          PIC  X(008).
                   15 BVVEW264-E-CMNTR-MSGRA       PIC  X(023).
                   15 BVVEW264-E-NETAPA-MNTRC      PIC  9(002).
                   15 BVVEW264-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW264-E-NU-CTRL-EMS       PIC  X(023).
                   15 BVVEW264-E-EVNTO             PIC  X(009).
HSM                15 BVVEW264-E-DATA-CONSULTA     PIC  X(010).
               10 BVVEW264-BLOCO-PERSISTENCIA.
                   15 BVVEW264-AREA                PIC  X(1000).
               10 BVVEW264-BLOCO-RETORNO.
                   15 BVVEW264-COD-RET             PIC  9(002).
      *                                                                *
               10 BVVEW264-ENTRADA-W513.
                   15 BVVEW264-E-CHAMADOR-W513     PIC  X(008).
                   15 BVVEW264-E-OPCAO-PESQ-W513   PIC  9(001).
                   15 BVVEW264-E-DT-MOV-INI-W513   PIC  X(010).
                   15 BVVEW264-E-DT-MOV-FIM-W513   PIC  X(010).
                   15 BVVEW264-E-HR-MOV-INI-W513   PIC  X(008).
                   15 BVVEW264-E-HR-MOV-FIM-W513   PIC  X(008).
                   15 BVVEW264-E-ORIGEM-W513.
                       20 BVVEW264-E-CBCO-ORIGE-W513 PIC  9(003).
                       20 BVVEW264-E-ISPB-ORIGE-W513 PIC  X(008).
                   15 BVVEW264-E-DESTINO.
                       20 BVVEW264-E-CBCO-DSTNO-W513    PIC  9(003).
                       20 BVVEW264-E-ISPB-DSTNO-W513    PIC  X(008).
                   15 BVVEW264-E-EVENTO-W513.
                       20 BVVEW264-E-CSIST-EXTER-W513   PIC  X(003).
                       20 BVVEW264-E-CEVNTO-MSG-W513    PIC  9(004).
                       20 BVVEW264-E-CFLUXO-MSG-W513    PIC  X(002).
                   15 BVVEW264-E-NU-CTRL-EMIS-W513      PIC  X(023).
                   15 BVVEW264-E-NU-CTRL-IF-W513        PIC  X(020).
MB0821             15 BVVEW264-E-CCONTR-W513            PIC  X(020).
                   15 BVVEW264-E-SITU-MSGEM-W513        PIC  9(003).
                   15 BVVEW264-E-FLUXO-MSGEM-W513       PIC  X(001).
                   15 BVVEW264-E-EIBTRMID-W513          PIC  X(008).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
