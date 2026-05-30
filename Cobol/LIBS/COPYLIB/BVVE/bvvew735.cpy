      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW735                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3735             *
      * DATA      : 10/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00459 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW735-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW735-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW735-E-DT-MOV-INI      :   DATA INICIAL DO MOVIMENTO       *
      * BVVEW735-E-DT-MOV-FIM      :   DATA FINAL DO MOVIMENTO         *
      * BVVEW735-E-HR-MOV-INI      :   HORA INICIAL DO MOVIMENTO       *
      * BVVEW735-E-HR-MOV-FIM      :   HORA FINAL DO MOVIMENTO         *
      * BVVEW735-E-ORIGEM.         :   ORIGEM                          *
      *    BVVEW735-E-CBCO-ORIGE   :       BANCO ORIGEM                *
      *    BVVEW735-E-ISPB-ORIGE   :       ISPB ORIGEM                 *
      * BVVEW735-E-DESTINO.        :   DESTINO                         *
      *    BVVEW735-E-CBCO-DSTNO   :       BANCO DESTINO               *
      *    BVVEW735-E-ISPB-DSTNO   :       ISPB DESTINO                *
      * BVVEW735-E-ARQUIVO.        :   CODIGO DO ARQUIVO CONSULTADO    *
      * BVVEW735-E-NU-CTRL-EMIS    :   NUMERO DE CONTROLE EMISSOR      *
      * BVVEW735-E-NU-CTRL-IF      :   NUMERO DE CONTROLE DA IF        *
      * BVVEW735-E-SITU-ARQUIVO    :   SITUACAO DO ARQUIVO             *
      * BVVEW735-E-FLUXO-ARQUIVO   :   FLUXO DO ARQUIVO                *
      *                                    E - ENVIADO                 *
      *                                    R - RECEBIDO                *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW735-REGISTRO.
               10 BVVEW735-BLOCO-ENTRADA.
                   15 BVVEW735-E-CHAMADOR          PIC  X(008).
                   15 BVVEW735-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW735-E-DT-MOV-INI        PIC  X(010).
                   15 BVVEW735-E-DT-MOV-FIM        PIC  X(010).
                   15 BVVEW735-E-HR-MOV-INI        PIC  X(008).
                   15 BVVEW735-E-HR-MOV-FIM        PIC  X(008).
                   15 BVVEW735-E-ORIGEM.
                       20 BVVEW735-E-CBCO-ORIGE    PIC  9(003).
                       20 BVVEW735-E-ISPB-ORIGE    PIC  X(008).
                   15 BVVEW735-E-DESTINO.
                       20 BVVEW735-E-CBCO-DSTNO    PIC  9(003).
                       20 BVVEW735-E-ISPB-DSTNO    PIC  X(008).
                   15 BVVEW735-E-ARQUIVO           PIC  X(007).
                   15 BVVEW735-E-NU-CTRL-EMIS      PIC  X(020).
                   15 BVVEW735-E-NU-CTRL-IF        PIC  X(020).
                   15 BVVEW735-E-SITU-ARQUIVO      PIC  9(003).
                   15 BVVEW735-E-FLUXO-ARQUIVO     PIC  X(001).
                   15 BVVEW735-E-EIBTRMID          PIC  X(008).
               10 BVVEW735-BLOCO-RESTART.
                   15 BVVEW735-CMNTRC-REST-INI     PIC  9(018).
                   15 BVVEW735-CMNTRC-REST-FIM     PIC  9(018).
               10 BVVEW735-BLOCO-SAIDA.
                   15 BVVEW735-S-CMNTRC-ARQUIVO    PIC  9(018)
                                                   OCCURS 12 TIMES.
               10 BVVEW735-BLOCO-RETORNO.
                   15 BVVEW735-COD-RET             PIC  9(002).
                   15 BVVEW735-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
