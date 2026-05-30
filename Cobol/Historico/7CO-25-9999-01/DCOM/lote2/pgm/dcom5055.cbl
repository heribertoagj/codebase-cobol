      *================================================================*
       IDENTIFICATION                       DIVISION.
      *================================================================*

       PROGRAM-ID.                          DCOM5055.
       AUTHOR.                              DUARTE.
       DATE-WRITTEN.                        26-04-2006.
      *DATE-COMPILLED.
      *================================================================*
      *
      * ALTERACAO ISENCAO DE CEDENTE POR PRODUTO / SUBPRODUTO
      * PROGRAMA LINEAR FOI MODULARIZADO POR PAULO FERRARINI  E
      * MAURICIO FRANCO
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** DCOM5055 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

       77  WRK-DINIC-VGCIA-ISENC-ANT          PIC  X(010) VALUE SPACES.
       77  WRK-DFIM-VGCIA-ISENC-ANT           PIC  X(010) VALUE SPACES.
       77  WRK-DINIC-VGCIA-ISENC-ENT          PIC  X(010) VALUE SPACES.
       77  WRK-DFIM-VGCIA-ISENC-ENT           PIC  X(010) VALUE SPACES.
       77  WRK-DINIC-VGCIA-ISENC-TAB          PIC  X(010) VALUE SPACES.
       77  WRK-DFIM-VGCIA-ISENC-TAB           PIC  X(010) VALUE SPACES.
       77  WRK-TIMESTAMP-ATU-DB2              PIC  X(010) VALUE SPACES.
FERR   77  WRK-CINDCD-MANUT-REG               PIC X(1).
FERR   77  WRK-CINDCD-REST-VLR-ANT            PIC X(1).
FERR   77  WRK-VREST-MAX-OPER-ANT             PIC S9(15)V9(2) COMP-3.
       77  WRK-ENTRE-VIGENCIAS                PIC  X(001) VALUE 'N'.
       77  IND-LKG                            PIC  9(003) VALUE ZEROS.
       77  WRK-HULT-ATULZ                     PIC  X(026) VALUE SPACES.

       01  WRK-CCHAVE-ELMTO-DESC.
           03  WRK-CCHAVE-ELMTO-DESC-1        PIC  9(003) VALUE ZEROS.
           03  WRK-CCHAVE-ELMTO-DESC-2        PIC  9(003) VALUE ZEROS.
4S2511*    03  WRK-CCHAVE-ELMTO-DESC-3        PIC  9(009) VALUE ZEROS.
4S2511     03  WRK-CCHAVE-ELMTO-DESC-3        PIC  X(009) VALUE SPACES.
           03  WRK-CCHAVE-ELMTO-DESC-5        PIC  X(010) VALUE SPACES.
           03  WRK-CCHAVE-ELMTO-DESC-6        PIC  9(003) VALUE ZEROS.
           03  FILLER                         PIC  X(014) VALUE SPACES.

       01  WRK-ENTRADA.
           03  WRK-COD-RETORNO-E              PIC  X(004).
           03  WRK-MSG-RETORNO-E              PIC  X(079).
           03  WRK-RESTART-E                  PIC  9(005).
           03  WRK-FLAG-E                     PIC  X(001).
           03  WRK-CFUNC-BDSCO-E              PIC  9(009).
           03  WRK-CTERM-E                    PIC  X(008).
           03  WRK-DADOS-E.
               05  WRK-CELMTO-DESC-COML-E     PIC  9(03).
               05  WRK-WCHAVE-PRINC-ELMTO-E   PIC  9(09).
               05  WRK-QDIG-SEQ-ELMTO-E       PIC  9(03).

       01  WRK-SAIDA.
           03  WRK-HEADER-S.
               05  WRK-COD-RETORNO-S          PIC  X(04).
               05  WRK-MSG-RETORNO-S          PIC  X(79).
               05  WRK-RESTART-S              PIC  9(05).
               05  WRK-FLAG-S                 PIC  X(01).
               05  WRK-NULT-SEQ-ELMTO-S       PIC  9(15).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA BOOK DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY  'I#DCOMSG'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM0000 ***'.
      *----------------------------------------------------------------*

       COPY  'I#DCOM48'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM0000 ***'.
      *----------------------------------------------------------------*

       COPY  'I#DCOM63'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM5055 ***'.
      *----------------------------------------------------------------*

       COPY  'I#DCOMX1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA FORMATACAO DE ERROS ***'.
      *----------------------------------------------------------------*

       COPY  'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM5055 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS  AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WS-AUX.
           03  WRK-ERRO                PIC S9(004)  COMP  VALUE  +0.
           03  WRK-MODULO              PIC  X(008)  VALUE  SPACES.
           03  WRK-SQLCA               PIC  X(136)  VALUE  SPACES.
           03  WRK-TIMESTAMP-ATUAL     PIC  X(026)  VALUE  SPACES.
           03  WRK-DATA-ANT            PIC  9(008)  VALUE  ZEROS.
           03  WRK-DATA-ATU            PIC  9(008)  VALUE  ZEROS.
           03  WRK-DATA-ATU-1          PIC  9(008)  VALUE  ZEROS.
           03  WRK-DATA-INI            PIC  9(008)  VALUE  ZEROS.
           03  WRK-DATA-FIM            PIC  9(008)  VALUE  ZEROS.
           03  WRK-DATA-FIM-ANT        PIC  9(008)  VALUE  ZEROS.
           03  WRK-DT-ENT              PIC  9(009)  COMP-3  VALUE  0.
           03  WRK-NUM-DIAS            PIC S9(005)  COMP-3  VALUE  +1.
           03  WRK-DT-SAI              PIC  9(009)  COMP-3  VALUE  0.
           03  WRK-NSEQ-PSSOA-DESC     PIC  9(003)  VALUE  ZEROS.
           03  WRK-NSEQ-PSSOA-DESC-ANT PIC  9(003)  VALUE  ZEROS.
           03  WRK-CABRGC-ISENC-DESC   PIC  9(003)  VALUE  ZEROS.
           03  WRK-CINDCD-REST-CDENT-ANT   PIC  X(001).
           03  WRK-48-OCORRENCIAS      PIC  X(084)  VALUE  SPACES.
           03  WRK-63-OCORRENCIAS      PIC  X(095)  VALUE  SPACES.

      *---------------------------------------------------------------*
       LINKAGE                         SECTION.
      *---------------------------------------------------------------*

       01  ENTRADA-ROTEADOR.
           03  ROTENT-HEADER.
               05  ROTENT-COD-RETORNO         PIC  X(004).
               05  ROTENT-MENSAGEM            PIC  X(079).
               05  ROTENT-RESTART             PIC  9(005).
               05  ROTENT-FLAG                PIC  X(001).
               05  ROTENT-CFUNC-BDSCO         PIC  9(009).
               05  ROTENT-CTERM               PIC  X(008).
           03  ROTENT-CONSISTENCIA.
4S2511*        05  ROTENT-CCNPJ-CPF           PIC  9(009).
4S2511         05  ROTENT-CCNPJ-CPF           PIC  X(09).
4S2511*        05  ROTENT-CFLIAL-CNPJ         PIC  9(004).
4S2511         05  ROTENT-CFLIAL-CNPJ         PIC  X(04).
               05  ROTENT-CCTRL-CNPJ-CPF      PIC  9(002).
               05  ROTENT-CTPO-ISENC-DESC     PIC  9(003).
               05  ROTENT-DINIC-ANT           PIC  X(010).
               05  ROTENT-DINIC-VGCIA-ISENC   PIC  X(010).
               05  ROTENT-DFIM-ANT            PIC  X(010).
               05  ROTENT-DFIM-VGCIA-ISENC    PIC  X(010).
               05  ROTENT-CPRODT              PIC  9(003).
               05  ROTENT-CSPROD-DESC-COML    PIC  9(003).
               05  ROTENT-CINDCD-REST-VLR     PIC  X(001).
               05  ROTENT-VREST-MAX-OPER      PIC  9(015)V99.

       01  SAIDA-ROTEADOR.
           03  ROTSAI-HEADER.
               05  ROTSAI-COD-RETORNO         PIC  X(004).
               05  ROTSAI-MSG-RETORNO         PIC  X(079).
               05  ROTSAI-RESTART             PIC  9(005).
               05  ROTSAI-FLAG                PIC  X(001).
           03  ROTSAI-CONSISTENCIA.
               05  ROTSAI-TAMANHO-ERRO        PIC  9(001).
               05  ROTSAI-TABELA-ERROS   OCCURS   09   TIMES.
                   07  ROTSAI-CAMPOS-ERRO     PIC  9(001).

       01  LKG-ERRO-AREA                      PIC  X(107).
       01  LKG-SQLCA                          PIC  X(136).

      *===============================================================*
       PROCEDURE  DIVISION             USING  ENTRADA-ROTEADOR
                                              SAIDA-ROTEADOR
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.
      *===============================================================*

      *---------------------------------------------------------------*
       000000-INICIO                   SECTION.
      *---------------------------------------------------------------*

           PERFORM  100000-INICIALIZAR.

           PERFORM  300000-PROCESSAR.

           PERFORM  900000-FINALIZAR.

      *---------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *---------------------------------------------------------------*

           INITIALIZE  SAIDA-ROTEADOR
                       48-ARGUMENTOS-ENTRADA
                       63-ARGUMENTOS-ENTRADA
                       X1-ARGUMENTOS-ENTRADA
                       LKG-ERRO-AREA
                       LKG-SQLCA
                       WRK-CCHAVE-ELMTO-DESC
                       WRK-ERRO.

           MOVE  1                     TO  ROTSAI-TAMANHO-ERRO.
           MOVE '0000'                 TO  ROTSAI-COD-RETORNO.
           MOVE SG-MENSAGEM(0001)      TO  ROTSAI-MSG-RETORNO.

           MOVE  'DCOM7999'            TO  WRK-MODULO.

           CALL  WRK-MODULO   USING   WRK-TIMESTAMP-ATUAL
                                      LKG-ERRO-AREA
                                      LKG-SQLCA.

           MOVE  WRK-TIMESTAMP-ATUAL(1:4)       TO  WRK-DATA-ATU(1:4).
           MOVE  WRK-TIMESTAMP-ATUAL(6:2)       TO  WRK-DATA-ATU(5:2).
           MOVE  WRK-TIMESTAMP-ATUAL(9:2)       TO  WRK-DATA-ATU(7:2).

           MOVE  ROTENT-DINIC-ANT        (1:2)  TO  WRK-DATA-ANT(7:2).
           MOVE  ROTENT-DINIC-ANT        (4:2)  TO  WRK-DATA-ANT(5:2).
           MOVE  ROTENT-DINIC-ANT        (7:4)  TO  WRK-DATA-ANT(1:4).

FERR       MOVE  ROTENT-DFIM-ANT      (1:2)  TO  WRK-DATA-FIM-ANT(7:2).
FERR       MOVE  ROTENT-DFIM-ANT      (4:2)  TO  WRK-DATA-FIM-ANT(5:2).
FERR       MOVE  ROTENT-DFIM-ANT      (7:4)  TO  WRK-DATA-FIM-ANT(1:4).

           MOVE  ROTENT-DINIC-VGCIA-ISENC(1:2)  TO  WRK-DATA-INI(7:2).
           MOVE  ROTENT-DINIC-VGCIA-ISENC(4:2)  TO  WRK-DATA-INI(5:2).
           MOVE  ROTENT-DINIC-VGCIA-ISENC(7:4)  TO  WRK-DATA-INI(1:4).

           MOVE  ROTENT-DFIM-VGCIA-ISENC(1:2)   TO  WRK-DATA-FIM(7:2).
           MOVE  ROTENT-DFIM-VGCIA-ISENC(4:2)   TO  WRK-DATA-FIM(5:2).
           MOVE  ROTENT-DFIM-VGCIA-ISENC(7:4)   TO  WRK-DATA-FIM(1:4).

      *---------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *---------------------------------------------------------------*

           PERFORM  400000-CONSISTIR-ENTRADA.

           PERFORM  500000-TRATAR-NEGOCIO.

      *---------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       400000-CONSISTIR-ENTRADA        SECTION.
      *---------------------------------------------------------------*

4S2511*    IF  ROTENT-CCNPJ-CPF   NOT NUMERIC OR
4S2511*        ROTENT-CCNPJ-CPF   EQUAL   ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF   EQUAL   SPACES OR
4S2511         ROTENT-CCNPJ-CPF   EQUAL   LOW-VALUES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(1)
               MOVE '0223'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0223)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  ROTENT-CTPO-ISENC-DESC   NOT NUMERIC OR
              (ROTENT-CTPO-ISENC-DESC   NOT EQUAL   1   AND   2)
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(2)
               MOVE '0538'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0538)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  ROTENT-DINIC-ANT   EQUAL   SPACES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(3)
               MOVE '0640'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0640)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  ROTENT-DINIC-VGCIA-ISENC   EQUAL   SPACES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(4)
               MOVE '0540'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0540)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  ROTENT-DFIM-VGCIA-ISENC   EQUAL   SPACES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(5)
               MOVE '0544'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0544)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  ROTENT-CPRODT   NOT NUMERIC OR
               ROTENT-CPRODT   EQUAL   ZEROS
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(6)
               MOVE '0115'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0115)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  ROTENT-CSPROD-DESC-COML   NOT NUMERIC OR
               ROTENT-CSPROD-DESC-COML   EQUAL   ZEROS
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(7)
               MOVE '0116'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0116)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  ROTENT-CINDCD-REST-VLR   NOT EQUAL 'S' AND 'N'
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(8)
               MOVE '0634'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0634)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF   ROTENT-CINDCD-REST-VLR   EQUAL   'S'
           AND (ROTENT-VREST-MAX-OPER    NOT NUMERIC
            OR  ROTENT-VREST-MAX-OPER    EQUAL   ZEROS)
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(8)
               MOVE '0659'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0659)  TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  WRK-DATA-INI EQUAL     WRK-DATA-ATU AND
               WRK-DATA-INI NOT EQUAL WRK-DATA-ANT AND
               WRK-DATA-FIM GREATER   WRK-DATA-INI
               MOVE  1                TO  ROTSAI-CAMPOS-ERRO(4)
               MOVE '0145'            TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0145) TO  ROTSAI-MSG-RETORNO
               PERFORM 900000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       500000-TRATAR-NEGOCIO           SECTION.
      *---------------------------------------------------------------*

           PERFORM  500010-OBTER-DATA-LOG.

           PERFORM  510020-OBTER-ULTIMA-ALTER.

           PERFORM  510000-VALIDAR-PERIODO.

           PERFORM  520000-VERIFICAR-ALTERACAO.

           PERFORM  800000-GRAVAR-LOG.

      *---------------------------------------------------------------*
       500000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       500010-OBTER-DATA-LOG           SECTION.
      *---------------------------------------------------------------*

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF.
4S2511*    MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ.
4S2511     MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF-ST.
4S2511     MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ-ST.
           MOVE ROTENT-CCTRL-CNPJ-CPF   TO 63-CCTRL-CNPJ-CPF.
4S2511     MOVE ROTENT-CCTRL-CNPJ-CPF   TO 63-CCTRL-CNPJ-CPF-ST.
           MOVE ROTENT-CTPO-ISENC-DESC  TO 63-CTPO-ISENC-DESC.
           MOVE ROTENT-CPRODT           TO 63-CPRODT.
           MOVE ROTENT-CSPROD-DESC-COML TO 63-CSPROD-DESC-COML.
           MOVE 'PARCIAL-05'            TO 63-INSTRUCAO.
           MOVE 'DCOM6815'              TO WRK-MODULO.

           CALL  WRK-MODULO             USING 63-ARGUMENTOS-ENTRADA
                                              63-RETORNO
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.

           IF  63-COD-RETORNO           NOT EQUAL ZEROS
               MOVE 63-COD-RETORNO      TO ROTSAI-COD-RETORNO
               MOVE 63-MENSAGEM         TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           MOVE 63RT-DINIC-VGCIA-ISENC(1) TO WRK-CCHAVE-ELMTO-DESC-5.

      *---------------------------------------------------------------*
       500010-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       510020-OBTER-ULTIMA-ALTER       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF.
4S2511*    MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ.
4S2511     MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF-ST.
4S2511     MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ-ST.
           MOVE ROTENT-CCTRL-CNPJ-CPF   TO 63-CCTRL-CNPJ-CPF.
4S2511     MOVE ROTENT-CCTRL-CNPJ-CPF   TO 63-CCTRL-CNPJ-CPF-ST.
           MOVE ROTENT-CTPO-ISENC-DESC  TO 63-CTPO-ISENC-DESC.
           MOVE ROTENT-CPRODT           TO 63-CPRODT.
           MOVE ROTENT-CSPROD-DESC-COML TO 63-CSPROD-DESC-COML.
           MOVE 'PARCIAL-07'            TO 63-INSTRUCAO.
           MOVE 'DCOM6815'              TO WRK-MODULO.

           CALL  WRK-MODULO             USING 63-ARGUMENTOS-ENTRADA
                                              63-RETORNO
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.

           IF  63-COD-RETORNO           NOT EQUAL ZEROS
               MOVE 63-COD-RETORNO      TO ROTSAI-COD-RETORNO
               MOVE 63-MENSAGEM         TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           MOVE   LENGTH OF 63-OCORRENCIAS(1)
                                             TO X1-WREG-LOG-ANTES-LEN.
           MOVE   63-OCORRENCIAS(1)          TO X1-WREG-LOG-ANTES-TEXT.

      *---------------------------------------------------------------*
       500010-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       510000-VALIDAR-PERIODO          SECTION.
      *---------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 63-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 63-CCTRL-CNPJ-CPF-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC      TO 63-CTPO-ISENC-DESC.
           MOVE  ROTENT-CPRODT               TO 63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML     TO 63-CSPROD-DESC-COML.
           MOVE  ROTENT-DINIC-ANT            TO 63-DINIC-VGCIA-ISENC.
           MOVE  'PARCIAL-01'                TO 63-INSTRUCAO.
           MOVE  'DCOM6815'                  TO WRK-MODULO.

           CALL  WRK-MODULO  USING
                             63-ARGUMENTOS-ENTRADA
                             63-RETORNO
                             LKG-ERRO-AREA
                             LKG-SQLCA.

           IF  63-COD-RETORNO NOT EQUAL ZEROS AND 2
               IF  63-COD-RETORNO EQUAL 3
                   MOVE '0660'      TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0660)
                                    TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0665'      TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0665)
                                    TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  900000-FINALIZAR
           END-IF.

FERR       MOVE 63RT-NSEQ-PSSOA-DESC(1)      TO WRK-NSEQ-PSSOA-DESC-ANT.

           MOVE  63-OCORRENCIAS(1)           TO WRK-63-OCORRENCIAS.
           MOVE  63RT-HULT-ATULZ(1)          TO WRK-HULT-ATULZ.

FERR       MOVE  63RT-CINDCD-REST-VLR(1)     TO WRK-CINDCD-REST-VLR-ANT.
FERR       MOVE  63RT-VREST-MAX-OPER(1)      TO WRK-VREST-MAX-OPER-ANT.

           MOVE  63RT-DFIM-VGCIA-ISENC(1)(1:2) TO
                 WRK-DFIM-VGCIA-ISENC-ENT(9:2).
           MOVE  '-'                           TO
                 WRK-DFIM-VGCIA-ISENC-ENT(8:1).
           MOVE  63RT-DFIM-VGCIA-ISENC(1)(4:2) TO
                 WRK-DFIM-VGCIA-ISENC-ENT(6:2).
           MOVE  '-'                           TO
                 WRK-DFIM-VGCIA-ISENC-ENT(5:1).
           MOVE  63RT-DFIM-VGCIA-ISENC(1)(7:4) TO
                 WRK-DFIM-VGCIA-ISENC-ENT(1:4).

           IF  WRK-DFIM-VGCIA-ISENC-ENT EQUAL WRK-TIMESTAMP-ATUAL(1:10)
               IF  ROTENT-DFIM-VGCIA-ISENC = 63RT-DFIM-VGCIA-ISENC(1)
                   MOVE  '0642'        TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0642)
                                       TO  ROTSAI-MSG-RETORNO
                  PERFORM  900000-FINALIZAR
               END-IF
           END-IF.

           MOVE ROTENT-DINIC-ANT               (1:2) TO
                WRK-DINIC-VGCIA-ISENC-ANT      (9:2)
           MOVE '.'                                  TO
                WRK-DINIC-VGCIA-ISENC-ANT      (8:1)
           MOVE ROTENT-DINIC-ANT               (4:2) TO
                WRK-DINIC-VGCIA-ISENC-ANT      (6:2)
           MOVE '.'                                  TO
                WRK-DINIC-VGCIA-ISENC-ANT      (5:1)
           MOVE ROTENT-DINIC-ANT               (7:4) TO
                WRK-DINIC-VGCIA-ISENC-ANT      (1:4)

           MOVE ROTENT-DINIC-VGCIA-ISENC       (1:2) TO
                WRK-DINIC-VGCIA-ISENC-ENT      (9:2)
           MOVE '.'                                  TO
                WRK-DINIC-VGCIA-ISENC-ENT      (8:1)
           MOVE ROTENT-DINIC-VGCIA-ISENC       (4:2) TO
                WRK-DINIC-VGCIA-ISENC-ENT      (6:2)
           MOVE '.'                                  TO
                WRK-DINIC-VGCIA-ISENC-ENT      (5:1)
           MOVE ROTENT-DINIC-VGCIA-ISENC       (7:4) TO
                WRK-DINIC-VGCIA-ISENC-ENT      (1:4)

           MOVE ROTENT-DFIM-ANT                (1:2) TO
                WRK-DFIM-VGCIA-ISENC-ANT       (9:2)
           MOVE '.'                                  TO
                WRK-DFIM-VGCIA-ISENC-ANT       (8:1)
           MOVE ROTENT-DFIM-ANT                (4:2) TO
                WRK-DFIM-VGCIA-ISENC-ANT       (6:2)
           MOVE '.'                                  TO
                WRK-DFIM-VGCIA-ISENC-ANT       (5:1)
           MOVE ROTENT-DFIM-ANT                (7:4) TO
                WRK-DFIM-VGCIA-ISENC-ANT       (1:4)

           MOVE ROTENT-DFIM-VGCIA-ISENC        (1:2) TO
                WRK-DFIM-VGCIA-ISENC-ENT       (9:2)
           MOVE '.'                                  TO
                WRK-DFIM-VGCIA-ISENC-ENT       (8:1)
           MOVE ROTENT-DFIM-VGCIA-ISENC        (4:2) TO
                WRK-DFIM-VGCIA-ISENC-ENT       (6:2)
           MOVE '.'                                  TO
                WRK-DFIM-VGCIA-ISENC-ENT       (5:1)
           MOVE ROTENT-DFIM-VGCIA-ISENC        (7:4) TO
                WRK-DFIM-VGCIA-ISENC-ENT       (1:4)

           MOVE 'N' TO WRK-ENTRE-VIGENCIAS.

           MOVE  'PARCIAL-41'                TO 63-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC      TO 63-CTPO-ISENC-DESC.
           MOVE  ROTENT-CPRODT               TO 63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML     TO 63-CSPROD-DESC-COML.
           MOVE  ZEROS                       TO 63-QTDE-A-DESPREZAR.
           MOVE  'DCOM6816'                  TO WRK-MODULO.

           CALL  WRK-MODULO                    USING
                                               63-ARGUMENTOS-ENTRADA
                                               63-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           PERFORM UNTIL (63-COD-RETORNO  NOT EQUAL ZEROS AND 2) OR
                         (WRK-ENTRE-VIGENCIAS EQUAL 'S')

               PERFORM VARYING IND-LKG FROM 1 BY 1
                       UNTIL (IND-LKG GREATER 63-QTDE-RETORNADA) OR
                             (WRK-ENTRE-VIGENCIAS EQUAL 'S')

                   MOVE 63RT-DINIC-VGCIA-ISENC(IND-LKG)(1:2) TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (9:2)
                   MOVE '.'                                  TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (8:1)
                   MOVE 63RT-DINIC-VGCIA-ISENC(IND-LKG)(4:2) TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (6:2)
                   MOVE '.'                                  TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (5:1)
                   MOVE 63RT-DINIC-VGCIA-ISENC(IND-LKG)(7:4) TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (1:4)

                   MOVE 63RT-DFIM-VGCIA-ISENC(IND-LKG)(1:2) TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (9:2)
                   MOVE '.'                                  TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (8:1)
                   MOVE 63RT-DFIM-VGCIA-ISENC(IND-LKG)(4:2) TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (6:2)
                   MOVE '.'                                  TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (5:1)
                   MOVE 63RT-DFIM-VGCIA-ISENC(IND-LKG)(7:4) TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (1:4)

                   IF ((WRK-DINIC-VGCIA-ISENC-ENT  NOT GREATER
                        WRK-DINIC-VGCIA-ISENC-TAB  AND
                        WRK-DFIM-VGCIA-ISENC-ENT   NOT LESS
                        WRK-DINIC-VGCIA-ISENC-TAB) OR
                       (WRK-DINIC-VGCIA-ISENC-ENT  NOT GREATER
                        WRK-DFIM-VGCIA-ISENC-TAB   AND
                        WRK-DFIM-VGCIA-ISENC-ENT   NOT LESS
                        WRK-DFIM-VGCIA-ISENC-TAB)  OR
                       (WRK-DINIC-VGCIA-ISENC-ENT  NOT LESS
                        WRK-DINIC-VGCIA-ISENC-TAB  AND
                        WRK-DFIM-VGCIA-ISENC-ENT   NOT GREATER
                        WRK-DFIM-VGCIA-ISENC-TAB)) AND
                      ((WRK-DINIC-VGCIA-ISENC-ENT  NOT EQUAL
                        WRK-DINIC-VGCIA-ISENC-ANT  OR
                        WRK-DFIM-VGCIA-ISENC-ENT   NOT EQUAL
                        WRK-DFIM-VGCIA-ISENC-ANT)  AND
                       (63RT-HULT-ATULZ(IND-LKG)   NOT EQUAL
                        WRK-HULT-ATULZ))
                        MOVE 'S' TO WRK-ENTRE-VIGENCIAS
                   END-IF

               END-PERFORM

               IF  63-COD-RETORNO      EQUAL 2 AND
                   WRK-ENTRE-VIGENCIAS EQUAL 'N'
4S2511*            MOVE ROTENT-CCNPJ-CPF        TO
4S2511*                                            63-CCNPJ-CPF
4S2511             MOVE ROTENT-CCNPJ-CPF        TO
4S2511                                             63-CCNPJ-CPF-ST
4S2511*            MOVE ROTENT-CFLIAL-CNPJ      TO
4S2511*                                            63-CFLIAL-CNPJ
4S2511             MOVE ROTENT-CFLIAL-CNPJ      TO
4S2511                                             63-CFLIAL-CNPJ-ST
                   MOVE ROTENT-CTPO-ISENC-DESC  TO
                                                63-CTPO-ISENC-DESC
                   MOVE ROTENT-CPRODT           TO
                                                63-CPRODT
                   MOVE ROTENT-CSPROD-DESC-COML TO
                                                63-CSPROD-DESC-COML
                   MOVE 63-PROXIMO-RESTART      TO
                                                63-QTDE-A-DESPREZAR
                   MOVE 'PARCIAL-41'            TO
                                                63-INSTRUCAO
                   MOVE 'DCOM6816'              TO
                                                WRK-MODULO
                   CALL  WRK-MODULO USING 63-ARGUMENTOS-ENTRADA
                                          63-RETORNO
                                          LKG-ERRO-AREA
                                          LKG-SQLCA
               ELSE
                   MOVE 3 TO 63-COD-RETORNO
               END-IF

           END-PERFORM.

           IF  63-COD-RETORNO NOT EQUAL ZEROS AND 2 AND 3
               MOVE '0647'         TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0647)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           IF  WRK-ENTRE-VIGENCIAS EQUAL 'S'
               MOVE '0653'         TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0653)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       510000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       520000-VERIFICAR-ALTERACAO      SECTION.
      *---------------------------------------------------------------*

           MOVE ROTENT-DINIC-ANT         (1:2) TO
                WRK-DINIC-VGCIA-ISENC-ENT(9:2).
           MOVE '-'                           TO
                WRK-DINIC-VGCIA-ISENC-ENT(8:1).
           MOVE ROTENT-DINIC-ANT         (4:2) TO
                WRK-DINIC-VGCIA-ISENC-ENT(6:2).
           MOVE '-'                           TO
                WRK-DINIC-VGCIA-ISENC-ENT(5:1).
           MOVE ROTENT-DINIC-ANT         (7:4) TO
                WRK-DINIC-VGCIA-ISENC-ENT(1:4).

           IF  WRK-DATA-INI <= WRK-DATA-ATU AND
               WRK-DINIC-VGCIA-ISENC-ENT <= WRK-TIMESTAMP-ATUAL(1:10)
               PERFORM  521000-ENCERRA-SUBPRODUTO
               PERFORM  800000-GRAVAR-LOG
               MOVE LENGTH OF 63-OCORRENCIAS(1)
                                               TO X1-WREG-LOG-ANTES-LEN
               MOVE 63-OCORRENCIAS(1)          TO X1-WREG-LOG-ANTES-TEXT
               MOVE WRK-DINIC-VGCIA-ISENC-ENT  TO WRK-TIMESTAMP-ATU-DB2
               MOVE WRK-TIMESTAMP-ATU-DB2    (9:2) TO
                    WRK-DINIC-VGCIA-ISENC-ENT(1:2)
               MOVE '.'                           TO
                    WRK-DINIC-VGCIA-ISENC-ENT(3:1)
               MOVE WRK-TIMESTAMP-ATU-DB2    (6:2) TO
                    WRK-DINIC-VGCIA-ISENC-ENT(4:2)
               MOVE '.'                           TO
                    WRK-DINIC-VGCIA-ISENC-ENT(6:1)
               MOVE WRK-TIMESTAMP-ATU-DB2    (1:4) TO
                    WRK-DINIC-VGCIA-ISENC-ENT(7:4)
               MOVE ROTENT-DINIC-ANT         (1:2) TO
                    WRK-DINIC-VGCIA-ISENC-ENT(9:2)
               MOVE '-'                           TO
                    WRK-DINIC-VGCIA-ISENC-ENT(8:1)
               MOVE ROTENT-DINIC-ANT         (4:2) TO
                    WRK-DINIC-VGCIA-ISENC-ENT(6:2)
               MOVE '-'                           TO
                    WRK-DINIC-VGCIA-ISENC-ENT(5:1)
               MOVE ROTENT-DINIC-ANT         (7:4) TO
                    WRK-DINIC-VGCIA-ISENC-ENT(1:4)
               PERFORM  521500-SOMA-DIA
           ELSE
               PERFORM  522000-EXCLUIR-SUBPRODUTO
               MOVE ROTENT-DINIC-VGCIA-ISENC TO
                                             WRK-DINIC-VGCIA-ISENC-ENT
           END-IF.

           IF  WRK-DATA-INI              <=
                                          WRK-DATA-ATU              AND
AQUI           ROTENT-DFIM-ANT        NOT =
                                          WRK-DATA-ATU              AND
               WRK-DATA-FIM               =
                                          WRK-DATA-ATU
               MOVE  '0643'          TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0643)
                                     TO  ROTSAI-MSG-RETORNO
           ELSE
               PERFORM  523000-INCLUIR-PESSOA
               PERFORM  524000-INCLUIR-SUBPRODUTO
           END-IF.

      *---------------------------------------------------------------*
       520000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       521000-ENCERRA-SUBPRODUTO       SECTION.
      *---------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF           TO  63-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF           TO  63-CCNPJ-CPF-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC     TO  63-CTPO-ISENC-DESC.
           MOVE  ROTENT-DINIC-ANT           TO  63-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CPRODT              TO  63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML    TO  63-CSPROD-DESC-COML.
           MOVE  WRK-DATA-ATU(1:4)          TO
                                              63-DFIM-VGCIA-ISENC(7:4).
           MOVE  '.'                        TO
                                              63-DFIM-VGCIA-ISENC(6:1).
           MOVE  WRK-DATA-ATU(5:2)          TO
                                              63-DFIM-VGCIA-ISENC(4:2).
           MOVE  '.'                        TO
                                              63-DFIM-VGCIA-ISENC(3:1).
           MOVE  WRK-DATA-ATU(7:2)          TO
                                              63-DFIM-VGCIA-ISENC(1:2).
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ         TO  63-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ         TO  63-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF      TO  63-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF      TO  63-CCTRL-CNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF      TO  63-CCTRL-CNPJ-CPF-ST.
           MOVE  ZEROS                      TO  63-CCLUB.
           MOVE  63RT-CINDCD-PSSOA-DESC(1)  TO  63-CINDCD-PSSOA-DESC.
FERR       MOVE  WRK-CINDCD-REST-VLR-ANT    TO  63-CINDCD-REST-VLR
FERR       MOVE  WRK-VREST-MAX-OPER-ANT     TO  63-VREST-MAX-OPER.
           MOVE  ROTENT-CFUNC-BDSCO         TO  63-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM               TO  63-CTERM
           MOVE  'PARCIAL-01'               TO  63-INSTRUCAO.
           MOVE  'DCOM6818'                 TO  WRK-MODULO

           CALL  WRK-MODULO   USING   63-ARGUMENTOS-ENTRADA
                                      63-RETORNO
                                      LKG-ERRO-AREA
                                      LKG-SQLCA.

           IF  63-COD-RETORNO NOT EQUAL ZEROS
               IF  63-COD-RETORNO EQUAL 3
                   MOVE '0683'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0683)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0644'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0644)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  900000-FINALIZAR
           END-IF.

4S2511*    MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 63-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 63-CCTRL-CNPJ-CPF-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC      TO 63-CTPO-ISENC-DESC.
           MOVE  ROTENT-CPRODT               TO 63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML     TO 63-CSPROD-DESC-COML.
           MOVE  ROTENT-DINIC-VGCIA-ISENC    TO 63-DINIC-VGCIA-ISENC.
           MOVE  'PARCIAL-10'                TO 63-INSTRUCAO.
           MOVE  'DCOM6815'                  TO WRK-MODULO.

           CALL  WRK-MODULO  USING
                             63-ARGUMENTOS-ENTRADA
                             63-RETORNO
                             LKG-ERRO-AREA
                             LKG-SQLCA.

           IF  63-COD-RETORNO NOT EQUAL ZEROS AND 2
               IF  63-COD-RETORNO EQUAL 3
                   MOVE '0660'      TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0660)
                                    TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0665'      TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0665)
                                    TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  900000-FINALIZAR
           END-IF.

           MOVE   LENGTH OF 63-OCORRENCIAS(1)
                                             TO X1-WREG-LOG-APOS-LEN.
           MOVE   63-OCORRENCIAS(1)          TO X1-WREG-LOG-APOS-TEXT.

      *---------------------------------------------------------------*
       521000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       521500-SOMA-DIA                 SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-DATA-ATU          TO  WRK-DT-ENT.

           CALL  'BRAD0025'   USING   WRK-DT-ENT
                                      WRK-NUM-DIAS
                                      WRK-DT-SAI.

           IF  RETURN-CODE   NOT EQUAL   ZEROS
               MOVE  '0714'            TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0714)  TO  ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           MOVE WRK-DT-SAI          TO WRK-DATA-ATU-1
           MOVE WRK-DATA-ATU-1(1:4) TO WRK-DINIC-VGCIA-ISENC-ENT(7:4).
           MOVE '.'                 TO WRK-DINIC-VGCIA-ISENC-ENT(6:1).
           MOVE WRK-DATA-ATU-1(5:2) TO WRK-DINIC-VGCIA-ISENC-ENT(4:2).
           MOVE '.'                 TO WRK-DINIC-VGCIA-ISENC-ENT(3:1).
           MOVE WRK-DATA-ATU-1(7:2) TO WRK-DINIC-VGCIA-ISENC-ENT(1:2).

      *---------------------------------------------------------------*
       521500-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       522000-EXCLUIR-SUBPRODUTO       SECTION.
      *---------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF           TO  63-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF           TO  63-CCNPJ-CPF-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC     TO  63-CTPO-ISENC-DESC.
           MOVE  ROTENT-DINIC-ANT           TO  63-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CPRODT              TO  63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML    TO  63-CSPROD-DESC-COML.
           MOVE  'PARCIAL-01'               TO  63-INSTRUCAO.
           MOVE  'DCOM6819'                 TO  WRK-MODULO

           CALL  WRK-MODULO   USING   63-ARGUMENTOS-ENTRADA
                                      63-RETORNO
                                      LKG-ERRO-AREA
                                      LKG-SQLCA.

           IF  63-COD-RETORNO   NOT EQUAL   ZEROS
               IF  63-COD-RETORNO EQUAL 3
                   MOVE '0649'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0649)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0099)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  900000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       522000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       523000-INCLUIR-PESSOA           SECTION.
      *---------------------------------------------------------------*

           MOVE  ROTENT-COD-RETORNO       TO  WRK-COD-RETORNO-E.
           MOVE  ROTENT-MENSAGEM          TO  WRK-MSG-RETORNO-E.
           MOVE  ROTENT-RESTART           TO  WRK-RESTART-E.
           MOVE  ROTENT-FLAG              TO  WRK-FLAG-E.
           MOVE  ROTENT-CFUNC-BDSCO       TO  WRK-CFUNC-BDSCO-E.
           MOVE  ROTENT-CTERM             TO  WRK-CTERM-E.

           IF  ROTENT-CTPO-ISENC-DESC EQUAL 1
               MOVE  13                   TO  WRK-CELMTO-DESC-COML-E
           ELSE
               MOVE  14                   TO  WRK-CELMTO-DESC-COML-E
           END-IF.

           MOVE  001                      TO  WRK-WCHAVE-PRINC-ELMTO-E.
           MOVE  003                      TO  WRK-QDIG-SEQ-ELMTO-E.
           MOVE  'DCOM5022'               TO  WRK-MODULO.

           CALL  WRK-MODULO            USING  WRK-ENTRADA
                                              WRK-SAIDA
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.

           IF  WRK-COD-RETORNO-S      NOT  EQUAL  ZEROS
               MOVE  '0657'               TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0657)     TO  ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           MOVE  WRK-NULT-SEQ-ELMTO-S       TO  WRK-NSEQ-PSSOA-DESC.

           MOVE  WRK-NSEQ-PSSOA-DESC        TO  48-NSEQ-PSSOA-DESC.
           MOVE  ROTENT-CCNPJ-CPF           TO  48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF           TO  48-CCNPJ-CPF-ST.
           MOVE  WRK-DINIC-VGCIA-ISENC-ENT  TO  48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC     TO  48-CTPO-ISENC-DESC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC    TO  48-DFIM-VGCIA-ISENC.
           MOVE  ROTENT-CFLIAL-CNPJ         TO  48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ         TO  48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF      TO  48-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF      TO  48-CCTRL-CNPJ-CPF-ST.
           MOVE  ZEROS                      TO  48-CCLUB.
           MOVE  1                          TO  48-CABRGC-ISENC-DESC.

4S2511*    IF  ROTENT-CFLIAL-CNPJ   EQUAL   ZEROS
4S2511     IF  ROTENT-CFLIAL-CNPJ   EQUAL   '0000' OR
4S2511         ROTENT-CFLIAL-CNPJ   EQUAL   SPACES OR
4S2511         ROTENT-CFLIAL-CNPJ   EQUAL   LOW-VALUES
               MOVE  'FI'                   TO  48-CINDCD-PSSOA-DESC
           ELSE
               MOVE  'PJ'                   TO  48-CINDCD-PSSOA-DESC
           END-IF.

           MOVE  'N'                        TO  48-CINDCD-REST-CDENT.
           MOVE  ROTENT-CFUNC-BDSCO         TO  48-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM               TO  48-CTERM
           MOVE  'DCOM6742'                 TO  WRK-MODULO

           CALL  WRK-MODULO   USING   48-ARGUMENTOS-ENTRADA
                                      48-RETORNO
                                      LKG-ERRO-AREA
                                      LKG-SQLCA.

           IF  63-COD-RETORNO   NOT EQUAL   ZEROS
               IF  63-COD-RETORNO EQUAL 4
                   MOVE '0050'         TO  ROTSAI-COD-RETORNO
                   MOVE 'ISENCAO PESSOA JA CADASTRADO'
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'         TO  ROTSAI-COD-RETORNO
                   MOVE 'ERRO AO ACESSAR OS DADOS ISENCAO PESSOA'
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  900000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       523000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       524000-INCLUIR-SUBPRODUTO       SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-NSEQ-PSSOA-DESC        TO  63-NSEQ-PSSOA-DESC.
4S2511*    MOVE  ROTENT-CCNPJ-CPF           TO  63-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF           TO  63-CCNPJ-CPF-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC     TO  63-CTPO-ISENC-DESC.
           MOVE  WRK-DINIC-VGCIA-ISENC-ENT  TO  63-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CPRODT              TO  63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML    TO  63-CSPROD-DESC-COML.
           MOVE  ROTENT-DFIM-VGCIA-ISENC    TO  63-DFIM-VGCIA-ISENC.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ         TO  63-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ         TO  63-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF      TO  63-CCTRL-CNPJ-CPF.
           MOVE  ZEROS                      TO  63-CCLUB.

4S2511*    IF  ROTENT-CFLIAL-CNPJ   EQUAL   ZEROS
4S2511     IF  ROTENT-CFLIAL-CNPJ   EQUAL   '0000' OR
4S2511         ROTENT-CFLIAL-CNPJ   EQUAL   SPACES OR
4S2511         ROTENT-CFLIAL-CNPJ   EQUAL   LOW-VALUES
               MOVE  'FI'                   TO  63-CINDCD-PSSOA-DESC
           ELSE
               MOVE  'PJ'                   TO  63-CINDCD-PSSOA-DESC
           END-IF.

           IF  ROTENT-CINDCD-REST-VLR EQUAL 'N'
               MOVE  'N'                    TO  63-CINDCD-REST-VLR
               MOVE  ZEROS                  TO  63-VREST-MAX-OPER
           ELSE
               MOVE  'S'                    TO  63-CINDCD-REST-VLR
               MOVE  ROTENT-VREST-MAX-OPER  TO  63-VREST-MAX-OPER
           END-IF.

           MOVE  ROTENT-CFUNC-BDSCO         TO  63-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM               TO  63-CTERM
           MOVE  'DCOM6817'                 TO  WRK-MODULO

           CALL  WRK-MODULO   USING   63-ARGUMENTOS-ENTRADA
                                      63-RETORNO
                                      LKG-ERRO-AREA
                                      LKG-SQLCA.

           IF  63-COD-RETORNO NOT EQUAL ZEROS
               IF  63-COD-RETORNO EQUAL 4
                   MOVE '0662'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0662)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0663'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0663)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  900000-FINALIZAR
           END-IF.

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF.
4S2511*    MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ.
4S2511     MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF-ST.
4S2511     MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ-ST.
           MOVE ROTENT-CCTRL-CNPJ-CPF   TO 63-CCTRL-CNPJ-CPF.
4S2511     MOVE ROTENT-CCTRL-CNPJ-CPF   TO 63-CCTRL-CNPJ-CPF-ST.
           MOVE ROTENT-CTPO-ISENC-DESC  TO 63-CTPO-ISENC-DESC.
           MOVE ROTENT-CPRODT           TO 63-CPRODT.
           MOVE ROTENT-CSPROD-DESC-COML TO 63-CSPROD-DESC-COML.
           MOVE 'PARCIAL-07'            TO 63-INSTRUCAO.
           MOVE 'DCOM6815'              TO WRK-MODULO.

           CALL  WRK-MODULO             USING 63-ARGUMENTOS-ENTRADA
                                              63-RETORNO
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.

           IF  63-COD-RETORNO           NOT EQUAL ZEROS
               MOVE 63-COD-RETORNO      TO ROTSAI-COD-RETORNO
               MOVE 63-MENSAGEM         TO ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

           MOVE   LENGTH OF 63-OCORRENCIAS(1)
                                             TO X1-WREG-LOG-APOS-LEN.
           MOVE   63-OCORRENCIAS(1)          TO X1-WREG-LOG-APOS-TEXT.

      *---------------------------------------------------------------*
       524000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       800000-GRAVAR-LOG               SECTION.
      *---------------------------------------------------------------*

      * GRAVAR  LOG TABELA SUBPRODUTO-PESSOA-ISENCAO

           MOVE  004                        TO X1-CELMTO-DESC-COML.

           IF ROTENT-CTPO-ISENC-DESC        EQUAL 1
              MOVE  058                     TO X1-CEVNTO-LOG-DESC
           ELSE
              MOVE  061                     TO X1-CEVNTO-LOG-DESC
           END-IF.

           MOVE  'A'                        TO X1-CINDCD-MANUT-REG.
           MOVE   ROTENT-CPRODT             TO WRK-CCHAVE-ELMTO-DESC-1.
           MOVE   ROTENT-CSPROD-DESC-COML   TO WRK-CCHAVE-ELMTO-DESC-2.
           MOVE   ROTENT-CCNPJ-CPF          TO WRK-CCHAVE-ELMTO-DESC-3.
           MOVE   ROTENT-CTPO-ISENC-DESC    TO WRK-CCHAVE-ELMTO-DESC-6.
           MOVE   WRK-CCHAVE-ELMTO-DESC     TO X1-CCHAVE-ELMTO-DESC.
           MOVE   'DCOM5055'                TO X1-CTRANS-PROG-ATULZ
           MOVE   'O'                       TO X1-CINDCD-MODLD-PROCM
           MOVE   237                       TO X1-CEMPR-INC
           MOVE   4130                      TO X1-CDEPDC
           MOVE   ROTENT-CFUNC-BDSCO        TO X1-CFUNC-BDSCO
           MOVE   ROTENT-CTERM              TO X1-CTERM
           MOVE   'DCOM5075'                TO WRK-MODULO

           CALL  WRK-MODULO   USING   X1-REGISTRO
                                      LKG-ERRO-AREA
                                      LKG-SQLCA.

           IF  X1-COD-RETORNO   NOT EQUAL   ZEROS
               MOVE '0215'         TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0215)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  900000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       800000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       900000-FINALIZAR                SECTION.
      *---------------------------------------------------------------*

           IF  ROTSAI-COD-RETORNO NOT EQUAL '0000' AND '0002' AND
                                            '0003' AND '0004' AND
                                            '0005'
               NEXT SENTENCE
           ELSE
               MOVE  'DCOM5055'    TO  ERR-PGM
               MOVE  SPACES        TO  LKG-SQLCA
               MOVE  ERRO-AREA     TO  LKG-ERRO-AREA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       900000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

