      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE3781.
       AUTHOR.     FABRICIO MASAKIYO MANCINI

      *================================================================*
      *                   7COMM  -  I N F O R M A T I C A              *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  BVVE3781                                     *
      *    MAPA........:  BVVE781                                      *
      *    TRANSACAO...:  VVKK                                         *
      *    PROGRAMADOR.:  FABRICIO MASAKIYO MANCINI         -  7COMM   *
      *    ANALISTA....:  FABRICIO MASAKIYO MANCINI         -  7COMM   *
      *    DATA........:  11/09/2020                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  MONITORAMENTO GEN0015 - FILTRO               *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TINSTC_FINCR_PARTC                          TMNFB012        *
      *    TSNLZD_MSGRA                                TMNFB043        *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    BVVEW782 - AREA DO MODULO DE LISTA DE ARQUIVOS              *
STATUS*    BVVEW785 - AREA DO MODULO DE LISTA DE SITUACOES DO ARQUIVO  *
      *    BVVEWE09 - AREA CONTENDO A ERRO-AREA P/ CDES (CD11)         *
      *    BVVEW400 - AREA PARA CONTROLE DE PERFIL                     *
      *    BVVEWERR - AREA PARA TRATAMENTO DE ERROS BVVE               *
      *    I#CALE01 - AREA PARA COMPONENTE CALENDARIO PARA LEGADO      *
      *    I#POOLB6 - AREA DE COMUNICACAO COM O MODULO CDES0111        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BVVE4000 - CONTROLE DE PERFIL                               *
      *    CALE1000 - CONSISTENCIA DE DATA                             *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: BVVE3726                                      *
      *    CHAMA       : BVVE3782 - LISTA DE MENSAGENS                 *
STATUS*                  BVVE3785 - LISTA DE SITUACOES DAS MENSAGENS   *
      *                  SENH1005 - SENHAS                             *
      *================================================================*

      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*
      *
      *================================================================*
       CONFIGURATION                   SECTION.
      *================================================================*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA DIVISION.
      *================================================================*
      *
      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING STORAGE  SECTION'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-PROGRAMA             PIC  X(008)         VALUE
              'BVVE3781'.
           05 WRK-BVVE3250             PIC  X(008)         VALUE
              'BVVE3250'.
           05 WRK-BVVE3782             PIC  X(008)         VALUE
              'BVVE3782'.
STATUS*    05 WRK-BVVE3785             PIC  X(008)         VALUE
STATUS*       'BVVE3785'.
           05 WRK-CALE1000             PIC  X(008)         VALUE
              'CALE1000'.
           05 WRK-TRANSACAO            PIC  X(004)         VALUE 'VVKK'.
           05 WRK-TRANS-3782           PIC  X(004)         VALUE 'VVKL'.
STATUS*    05 WRK-TRANS-3785           PIC  X(004)         VALUE 'NFBV'.
           05 WRK-TMNFB012             PIC  X(018)         VALUE
              'TINSTC_FINCR_PARTC'.
STATUS*    05 WRK-TMNFB043             PIC  X(018)         VALUE
STATUS*       'TSNLZD_MSGRA'.
           05 WRK-TITULO               PIC  X(034)         VALUE
              '      MONITORAMENTO GEN0015       '.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA DE FORMATACAO DE DATA/HORA'.
      *----------------------------------------------------------------*

       01  WRK-FORMATA-DT-HORA.
           05 WRK-HORA                 PIC  X(008)         VALUE SPACES.
           05 WRK-DATA                 PIC  X(010)         VALUE SPACES.
           05 WRK-ABSTIME              PIC  9(015)         VALUE ZEROS.

           05 WRK-DATA-CALE-INI.
               10 WRK-DATA-CALE-ANO-I  PIC  X(004)         VALUE SPACES.
               10 WRK-DATA-CALE-MES-I  PIC  X(002)         VALUE SPACES.
               10 WRK-DATA-CALE-DIA-I  PIC  X(002)         VALUE SPACES.

           05 WRK-DATA-CALE-FIN.
               10 WRK-DATA-CALE-ANO-F  PIC  X(004)         VALUE SPACES.
               10 WRK-DATA-CALE-MES-F  PIC  X(002)         VALUE SPACES.
               10 WRK-DATA-CALE-DIA-F  PIC  X(002)         VALUE SPACES.

           05 WRK-AUX-DATA-X-8         PIC  X(008)         VALUE SPACES.

           05 WRK-FLG-DATA-CALE        PIC  X(001)         VALUE SPACES.
               88 WRK-FLG-DT-INI                           VALUE 'I'.
               88 WRK-FLG-DT-FIN                           VALUE 'F'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA MENSAGENS'.
      *----------------------------------------------------------------*

       01  WRK-AREA-MENSAGEM.
           05 WRK-MSG-ERRO             PIC  X(079)         VALUE SPACES.
           05 WRK-MSG-ERRO-1           PIC  X(079)         VALUE
              'VVKK - ACESSO NEGADO, FAVOR ACESSAR PELO SENHAS'.
           05 WRK-MSG001               PIC  X(079)         VALUE
              'DIGITE OS DADOS E TECLE <ENTER>'.
           05 WRK-MSG002               PIC  X(079)         VALUE
              'DADOS INVALIDOS'.
           05 WRK-MSG007               PIC  X(079) VALUE
                   'DIGITE A DATA E AO MENOS UM DOS DEMAIS FILTROS'.
           05 WRK-MSG011               PIC  X(079)         VALUE
              'DATA INICIAL DEVE SER MENOR OU IGUAL A DATA FINAL'.
           05 WRK-MSG012               PIC  X(079)         VALUE
              'ESCOLHA UMA DAS OPÁOES'.
           05 WRK-MSG013               PIC  X(079)         VALUE
              'SELECIONE A OPCAO COM X'.
STATUS*    05 WRK-MSG-PF11.
STATUS*        10 FILLER               PIC  X(004)         VALUE SPACES.
STATUS*        10 FILLER               PIC  X(028)         VALUE
STATUS*           'PRESSIONE <PF11> PARA LISTAR'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '* INDICADORES DE NULIDADE *'.
      *----------------------------------------------------------------*

       01  WRK-BVVETEST-NULL.
           03 WRK-CBCO-CONGL-NULL      PIC S9(004) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE SENHAS *'.
      *----------------------------------------------------------------*

       01  WRK-SENH1005                PIC  X(008)         VALUE
           'SENH1005'.
       01  WRK-AREA-SENH1005.
           05  WRK-VERSAO              PIC  9(002)         VALUE 1.
           05  WRK-MENSAGEM.
               10  WRK-COD-MENSAGEM    PIC  9(003)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE SPACES.
               10  WRK-TEXTO-MENSAGEM  PIC  X(079)         VALUE SPACES.
           05  WRK-SISTEMA             PIC  X(004)         VALUE SPACES.
           05  WRK-NIVEL               PIC  X(001)         VALUE SPACES.
               88  WRK-MENU-SENHAS     VALUE 'P'.
               88  WRK-MENU-BVVE       VALUE 'S'.
           05  WRK-ACAO                PIC  X(001)         VALUE SPACES.
               88  WRK-ENCERRA         VALUE 'E'.
               88  WRK-CONTINUA        VALUE 'C'.
               88  WRK-TRATA-ERRO      VALUE 'T'.
           05  WRK-LINHA-COMANDO       PIC  X(068)         VALUE SPACES.
           05  WRK-IMPRESSORA-1005     PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(096)         VALUE SPACES.

      *----------------------------------------------------------------*
        01  FILLER                      PIC  X(050)        VALUE
            '* AREA DE COMUNICACAO COM O CALE1000 *'.
      *----------------------------------------------------------------*

       COPY 'I#CALE01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DO TECLADO *'.
      *----------------------------------------------------------------*

       COPY DFHAID.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*   AREA PARA SETAR ATRIBUTOS  *'.
      *----------------------------------------------------------------*

       COPY DFHBMSCA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TRATAMENTO DE ERROS *'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-ERRO.
           COPY BVVEWERR.
      *                                                                *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO CDES0111 *'.
      *----------------------------------------------------------------*
      *                                                                *
       COPY 'I#POOLB6'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DO MAPA *'.
      *----------------------------------------------------------------*

       COPY 'BVVE781'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE LAYOUT BVVE3250 *'.
      *----------------------------------------------------------------*

        01  WRK-AREA-BVVE3250.
            05 WRK-3250-CFUNC-BDSCO     PIC  X(007)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE LAYOUT BVVE3726 *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-BVVE3726.
           05 WRK-3726-CFUNC-BDSCO     PIC  X(007)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE LAYOUT BVVE3782 *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-BVVE3782.
           COPY 'BVVEW782'.

STATUS*----------------------------------------------------------------*
.     *01  FILLER                      PIC  X(050)         VALUE
.     *    '* AREA DE LAYOUT BVVE3785 *'.
.     *----------------------------------------------------------------*
.     *
.     *01  WRK-AREA-BVVE3785.
.     *   -COPY 'BVVEW785'.
STATUS*   -COPY 'BVVEWE09'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE LAYOUT DO MODULO *'.
      *----------------------------------------------------------------*

      *01  WRK-COMMAREA.
      *    05 WRK-COMM-CFUNC-BDSCO     PIC  X(007)         VALUE SPACES.
      *    05 WRK-COMM-FASE            PIC  9(001)         VALUE ZEROS.
      *    05 WRK-COMM-OPCAO           PIC  9(002)         VALUE ZEROS.
      *                                                                *
       01  WRK-COMMAREA.
           05 WRK-COMM-OPCAO           PIC  9(002)         VALUE ZEROS.
           05 WRK-COMM-CFUNC-BDSCO     PIC  X(007)         VALUE SPACES.
           05 WRK-COMM-EIBTRMID        PIC  X(008)         VALUE SPACES.
           05 WRK-COMM-FASE            PIC  9(001)         VALUE ZEROS.
      *                                                                *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE TMNFB012
           END-EXEC.

STATUS*   -EXEC SQL
STATUS*        INCLUDE TMNFB043
STATUS*    END-EXEC.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'FIM DA WORKING STORAGE     SECTION'.
      *----------------------------------------------------------------*
      *
      *================================================================*
       LINKAGE                         SECTION.
      *================================================================*

       01  DFHCOMMAREA                 PIC  X(5000).
      *
      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*
      *
      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           MOVE '0010'                 TO ERR-LOCAL.

           PERFORM 1000-INICIALIZAR.

           IF  EIBCALEN                EQUAL ZEROS
               PERFORM 1100-RECEBER-AREA-SENHAS
               PERFORM 3100-OBTER-DATA-HORA
AJG            MOVE WRK-DATA(1:2)      TO DIAINII
AJG            MOVE WRK-DATA(4:2)      TO MESINII
AJG            MOVE WRK-DATA(7:4)      TO ANOINII
               PERFORM 3300-MONTAR-FASE-2
               MOVE -1                 TO ISPBDTNL
               MOVE 'X'                TO OPCNAOO
               PERFORM 3000-ENVIAR-TELA
           END-IF.

           EVALUATE EIBTRNID
               WHEN WRK-TRANS-3782
                   PERFORM 1500-RETORNO-BVVE3782
STATUS*       -WHEN WRK-TRANS-3785
STATUS*           -PERFORM 1600-RETORNO-BVVE3785
               WHEN WRK-TRANSACAO
                   MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
               WHEN OTHER
                   MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
                   PERFORM 3200-MONTAR-FASE-1
                   PERFORM 3000-ENVIAR-TELA
           END-EVALUATE.

           PERFORM 4000-RECEBER-TELA.

           EVALUATE EIBAID
               WHEN DFHPF2
                   PERFORM 4100-TRATAR-PF2
               WHEN DFHPF3
                   PERFORM 4200-TRATAR-PF3
               WHEN DFHPF5
                   PERFORM 4300-TRATAR-PF5
               WHEN DFHPF9
                   PERFORM 4303-TRATAR-PF9
               WHEN DFHPF10
                   PERFORM 4305-TRATAR-PF10
STATUS*        WHEN DFHPF11
STATUS*           -PERFORM 4310-TRATAR-PF11
               WHEN DFHENTER
                   PERFORM 4400-TRATAR-ENTER
               WHEN DFHCLEAR
                   MOVE 'VVKK - FIM DE TRANSACAO'
                                       TO WRK-MSG-ERRO
                   PERFORM 5000-ENCERRAR-TRANSACAO
               WHEN OTHER
                   PERFORM 4500-TRATAR-PF-INVALIDA
           END-EVALUATE.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZACAO DO PROGRAMA                            *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE '0020'                 TO ERR-LOCAL.

           EXEC CICS HANDLE
               ABEND
               LABEL (9400-ERRO-CICS)
           END-EXEC.

           MOVE LOW-VALUES             TO BVVE781O.

           INITIALIZE                  ERRO-AREA
                                       WRK-AREA-BVVE3726
                                       WRK-AREA-BVVE3782
                                       WRK-COMMAREA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RECEBE AREA DO SENHAS                                   *
      *----------------------------------------------------------------*
       1100-RECEBER-AREA-SENHAS        SECTION.
      *----------------------------------------------------------------*

           MOVE '0030'                 TO ERR-LOCAL.

           EXEC CICS
                RETRIEVE
                INTO   (WRK-AREA-SENH1005)
                LENGTH (LENGTH OF WRK-AREA-SENH1005)
                NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              IF EIBRESP               EQUAL 29
                 MOVE WRK-MSG-ERRO-1   TO WRK-MSG-ERRO
                 PERFORM 5000-ENCERRAR-TRANSACAO
              END-IF

              MOVE '0040'              TO ERR-LOCAL

              PERFORM 9400-ERRO-CICS
              MOVE    LOW-VALUES       TO BVVE781O
              PERFORM 3000-ENVIAR-TELA
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TRATA O RETORNO DO BVVE3782 - LISTA DE MENSAGENS               *
      *----------------------------------------------------------------*
       1500-RETORNO-BVVE3782           SECTION.
      *----------------------------------------------------------------*

           MOVE '0050'                 TO ERR-LOCAL.

           MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-AREA-BVVE3782.

           MOVE BVVEW782-E-DT-MOV-INI(1:2) TO DIAINII.
           MOVE BVVEW782-E-DT-MOV-INI(4:2) TO MESINII.
           MOVE BVVEW782-E-DT-MOV-INI(7:4) TO ANOINII.
           MOVE BVVEW782-E-ISPB-ORIGE      TO ISPBORII.
           MOVE BVVEW782-E-ISPB-DSTNO      TO ISPBDTNO.
           MOVE BVVEW782-E-NOME-ARQUIVO    TO ARQUIVOI.
           MOVE BVVEW782-E-EVENTO          TO EVENTOI.

           PERFORM 3300-MONTAR-FASE-2.

           IF BVVEW782-COD-RET         EQUAL ZEROS
               MOVE WRK-MSG001         TO MENSAGO
           ELSE
               MOVE BVVEW782-MSG-RET   TO MENSAGO
           END-IF.

           MOVE -1                     TO ISPBDTNL.
           MOVE 'X'                    TO OPCNAOO

           PERFORM 3000-ENVIAR-TELA.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

STATUS*----------------------------------------------------------------*
.     * TRATA O RETORNO DO BVVE3785 - LISTA DE SITUACOES DAS MENSAGENS *
.     *----------------------------------------------------------------*
.     *1600-RETORNO-BVVE3785           SECTION.
.     *----------------------------------------------------------------*
.     *
.     *   -MOVE '0060'                 TO ERR-LOCAL.
.     *
.     *   -MOVE DFHCOMMAREA(1:EIBCALEN)
.     *                                TO WRK-AREA-BVVE3785.
.     *
.     *   -MOVE BVVEW785-AREA-CHAMADOR TO WRK-COMMAREA
.     *   -MOVE BVVEW785-MAPA-CHAMADOR(1: LENGTH OF BVVE781O)
.     *                                TO BVVE781O.
.     *
.     *   -MOVE BVVEWE09-ERRO-AREA     OF WRK-AREA-BVVE3785
.     *                                TO ERRO-AREA.
.     *
.     *   -IF ERR-RETURN-CODE          EQUAL ZEROS
.     *       -MOVE BVVEW785-CSNLZD-MSGRA
.     *                                TO STATUSO
.     *       -MOVE BVVEW785-RSNLZD-MSGRA(1: LENGTH OF DESCSITO)
.     *                                TO DESCSITO
.     *       -MOVE WRK-MSG001         TO MENSAGO
.     *    ELSE
.     *       -MOVE ERR-TEXTO-MSG      TO MENSAGO
.     *    END-IF.
.     *
.     *   -MOVE -1                     TO STATUSL.
.     *
.     *   -PERFORM 3000-ENVIAR-TELA.
.     *
.     *----------------------------------------------------------------*
.     *1600-99-FIM.                    EXIT.
STATUS*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSISTENCIA DOS CAMPOS INFORMADOS NA TELA                  *
      *----------------------------------------------------------------*
       2000-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           MOVE '0070'                 TO ERR-LOCAL.

      *** EMISSOR ******************************************************
           IF ISPBORIO                 NOT NUMERIC
               MOVE SPACES             TO ISPBORIO
           END-IF.

           IF ISPBDTNO                 NOT NUMERIC
               MOVE SPACES             TO ISPBDTNO
           END-IF.

           IF NCTRLEMO                 EQUAL LOW-VALUES
               MOVE SPACES             TO NCTRLEMO
           END-IF.

           IF EVENTOO                  EQUAL LOW-VALUES
               MOVE SPACES             TO EVENTOO
           END-IF.

           IF ARQUIVOO                 EQUAL LOW-VALUES
               MOVE SPACES             TO ARQUIVOO
           END-IF.

           INITIALIZE                  TMNFB012.

           IF OPCNAOO                  EQUAL LOW-VALUES
              MOVE SPACES              TO OPCNAOO
           END-IF

           IF OPCSIMO                  EQUAL LOW-VALUES
              MOVE SPACES              TO OPCSIMO
           END-IF

           IF OPCNAOO                  EQUAL SPACES AND
              OPCSIMO                  EQUAL SPACES
              MOVE -1                  TO OPCNAOL
              MOVE WRK-MSG012          TO MENSAGO
              PERFORM 3000-ENVIAR-TELA
           END-IF.

           IF OPCNAOO                  NOT EQUAL 'X' AND SPACES
              MOVE -1                  TO OPCNAOL
              MOVE WRK-MSG013          TO MENSAGO
              PERFORM 3000-ENVIAR-TELA
           END-IF.

           IF OPCSIMO                  NOT EQUAL 'X' AND SPACES
              MOVE -1                  TO OPCSIML
              MOVE WRK-MSG013          TO MENSAGO
              PERFORM 3000-ENVIAR-TELA
           END-IF.

           IF ISPBORIO                 IS NUMERIC
               MOVE ISPBORIO           TO CISPB-PARTC-SIST
                                       OF TMNFB012
               PERFORM 2120-VALIDAR-ISPB

               IF (SQLCODE             EQUAL +100)
                   MOVE WRK-MSG002     TO MENSAGO
                   MOVE -1             TO ISPBORIL
                   PERFORM 3000-ENVIAR-TELA
               END-IF
           ELSE
               MOVE SPACES             TO ISPBORIO
      **********OVE -1                 TO ISPBORIL
      **********OVE WRK-MSG002         TO MENSAGO
      **********ERFORM 3000-ENVIAR-TELA
           END-IF.

      *** DESTINATARIO *************************************************
           INITIALIZE                  TMNFB012.

           IF ISPBDTNO                 IS NUMERIC
               MOVE ISPBDTNO           TO CISPB-PARTC-SIST
                                       OF TMNFB012
               PERFORM 2120-VALIDAR-ISPB

               IF (SQLCODE             EQUAL +100)
                   MOVE WRK-MSG002     TO MENSAGO
                   MOVE -1             TO ISPBDTNL
                   PERFORM 3000-ENVIAR-TELA
               END-IF
           ELSE
               MOVE SPACES             TO ISPBDTNO
           END-IF.

           IF NCTRLEMO EQUAL SPACES AND
              ISPBORIO EQUAL SPACES AND
              ISPBDTNO EQUAL SPACES AND
              ARQUIVOO EQUAL SPACES AND
              EVENTOO  EQUAL SPACES
              MOVE WRK-MSG007     TO MENSAGO
              MOVE -1             TO ISPBDTNL
              PERFORM 3000-ENVIAR-TELA
           END-IF.



STATUS*** SITUACAO DA MENSAGEM *****************************************
.     *   -EXEC CICS BIF
.     *        DEEDIT
.     *        FIELD(STATUSI)
.     *    END-EXEC.
.     *
.     *   -IF STATUSI                  NOT NUMERIC
.     *       -MOVE ZEROS              TO STATUSO
.     *    END-IF.
.     *
.     *   -IF STATUSO                  NOT EQUAL ZEROS
.     *       -PERFORM 2130-VALIDAR-SITUACAO
.     *    ELSE
.     *       -MOVE SPACES             TO STATUSI
.     *                                   DESCSITO
STATUS*    END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR ISPB                                                *
      *----------------------------------------------------------------*
       2120-VALIDAR-ISPB               SECTION.
      *----------------------------------------------------------------*

           MOVE '0080'                 TO ERR-LOCAL.

           EXEC SQL
               SELECT  CBCO_CONGL
                 INTO :TMNFB012.CBCO-CONGL
                           :WRK-CBCO-CONGL-NULL
                 FROM DB2PRD.TINSTC_FINCR_PARTC
                WHERE CISPB_PARTC_SIST  = :TMNFB012.CISPB-PARTC-SIST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET  ERR-DB2-SELECT     TO TRUE
               MOVE WRK-TMNFB012       TO BVVEWERR-DB2-TABELA
               MOVE '0090'             TO ERR-LOCAL
               PERFORM 9300-ERRO-DB2
               MOVE BVVEWERR-ERR-DB2   TO MENSAGO
               PERFORM 3000-ENVIAR-TELA
           END-IF.

           IF WRK-CBCO-CONGL-NULL      NOT EQUAL ZEROS
               MOVE ZEROS              TO CBCO-CONGL
                                       OF TMNFB012
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

STATUS*----------------------------------------------------------------*
.     *    VALIDAR A SITUACAO DA MENSAGEM                              *
.     *----------------------------------------------------------------*
.     *2130-VALIDAR-SITUACAO           SECTION.
.     *----------------------------------------------------------------*
.     *
.     *   -MOVE '0100'                 TO ERR-LOCAL.
.     *
.     *   -INITIALIZE                  TMNFB043.
.     *
.     *   -MOVE STATUSO                TO CSNLZD-MSGRA
.     *                                OF TMNFB043.
.     *
.     *   -EXEC SQL
.     *        -SELECT RSNLZD_MSGRA
.     *           INTO :TMNFB043.RSNLZD-MSGRA
.     *           FROM DB2PRD.TSNLZD_MSGRA
.     *         WHERE  CSNLZD_MSGRA   = :TMNFB043.CSNLZD-MSGRA
.     *          WITH UR
.     *    END-EXEC.
.     *
.     *   -IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
.     *       (SQLWARN0                EQUAL 'W')
.     *       -SET  ERR-DB2-SELECT     TO TRUE
.     *       -MOVE WRK-TMNFB043       TO BVVEWERR-DB2-TABELA
.     *       -MOVE '0110'             TO ERR-LOCAL
.     *       -PERFORM 9300-ERRO-DB2
.     *       -MOVE BVVEWERR-ERR-DB2   TO MENSAGO
.     *       -PERFORM 3000-ENVIAR-TELA
.     *    END-IF.
.     *
.     *   -IF SQLCODE                  EQUAL 100
.     *       -MOVE SPACES             TO DESCSITO
.     *       -MOVE -1                 TO STATUSL
.     *       -MOVE WRK-MSG002         TO MENSAGO
.     *       -PERFORM 3000-ENVIAR-TELA
.     *    ELSE
.     *       -MOVE RSNLZD-MSGRA       OF TMNFB043
.     *                                TO DESCSITO
.     *    END-IF.
.     *
.     *----------------------------------------------------------------*
.     *2130-99-FIM.                    EXIT.
STATUS*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CHAMA O MODULO BVVE3782 P/ MONTAR LISTA CONFORME O FILTRO   *
      *----------------------------------------------------------------*
       2900-MONTAR-LISTA               SECTION.
      *----------------------------------------------------------------*

           MOVE '0120'                 TO ERR-LOCAL.

           INITIALIZE BVVEW782-E-DT-MOV-FIM

           MOVE WRK-PROGRAMA           TO BVVEW782-E-CHAMADOR.

           STRING                      DIAINIO '.'
                                       MESINIO '.'
                                       ANOINIO
                                       DELIMITED BY SIZE
                                       INTO BVVEW782-E-DT-MOV-INI.


           MOVE ISPBORIO               TO BVVEW782-E-ISPB-ORIGE.
           MOVE ISPBDTNO               TO BVVEW782-E-ISPB-DSTNO.
           MOVE NCTRLEMO               TO BVVEW782-E-NU-CTRL-EMIS.

           IF   OPCNAOO EQUAL 'X'
                MOVE '1'               TO BVVEW782-E-TP-CONSULTA
           ELSE
                MOVE '2'               TO BVVEW782-E-TP-CONSULTA
           END-IF.

           MOVE ARQUIVOO               TO BVVEW782-E-NOME-ARQUIVO
           MOVE EVENTOO                TO BVVEW782-E-EVENTO.

STATUS*   -IF STATUSO                  IS NUMERIC
.     *       -MOVE STATUSO            TO BVVEW782-E-SITU-MSGEM
.     *    ELSE
.     *       -MOVE ZEROS              TO BVVEW782-E-SITU-MSGEM
STATUS*    END-IF.

           MOVE WRK-PROGRAMA           TO BVVEW782-E-EIBTRMID.

           EXEC CICS XCTL
               PROGRAM (WRK-BVVE3782)
               COMMAREA(WRK-AREA-BVVE3782)
               LENGTH  (LENGTH OF WRK-AREA-BVVE3782)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE WRK-BVVE3782       TO BVVEWERR-CICS-ACESSADO
               PERFORM 9400-ERRO-CICS
           END-IF.

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ENVIA INTERFACE PARA INTERACAO COM USUARIO                  *
      *----------------------------------------------------------------*
       3000-ENVIAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE '0130'                 TO ERR-LOCAL.

           PERFORM 3100-OBTER-DATA-HORA.

           MOVE WRK-TITULO             TO TITULOO.

STATUS*   -IF STATUSI                  EQUAL SPACES OR LOW-VALUES
STATUS*       -MOVE WRK-MSG-PF11       TO DESCSITO
STATUS*    END-IF.

           IF  WRK-COMM-FASE           EQUAL 2
               MOVE DFHBMPRF           TO DIAINIA
                                          MESINIA
                                          ANOINIA

               MOVE -1                 TO ISPBDTNL
           END-IF.

           EXEC CICS SEND
               MAP    ('BVVE781')
               MAPSET ('BVVE781')
               FROM   (BVVE781O)
               ERASE
               CURSOR
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID  ('VVKK')
               COMMAREA (WRK-COMMAREA)
               LENGTH   (LENGTH OF WRK-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTEM E FORMATA DATA E HORA CORRENTE                        *
      *----------------------------------------------------------------*
       3100-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           MOVE '0140'                 TO ERR-LOCAL.

           EXEC CICS ASKTIME
               ABSTIME     (WRK-ABSTIME)
           END-EXEC.

           EXEC CICS FORMATTIME
               ABSTIME     (WRK-ABSTIME)
               DATESEP
               DDMMYYYY    (WRK-DATA)
               TIMESEP
               TIME        (WRK-HORA)
           END-EXEC.

           MOVE WRK-HORA               TO HORSISO.
           MOVE WRK-DATA               TO DTASISO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-MONTAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           MOVE '0150'                 TO ERR-LOCAL.

           MOVE 1                      TO WRK-COMM-FASE.

           MOVE LOW-VALUES             TO DIAINII
                                          MESINII
                                          ANOINII
                                          ISPBORII
                                          ISPBDTNI
STATUS*                                   STATUSI
STATUS*                                   DESCSITI.

           MOVE -1                     TO DIAINIL.
           MOVE WRK-MSG001             TO MENSAGO.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-MONTAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           MOVE '0160'                 TO ERR-LOCAL.

           MOVE 2                      TO WRK-COMM-FASE.

           MOVE WRK-MSG001             TO MENSAGO.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    RECEBER AS INFORMACOES DA TELA DE INTERACAO COM O USUARIO   *
      *----------------------------------------------------------------*
       4000-RECEBER-TELA               SECTION.
      *----------------------------------------------------------------*

           MOVE '0170'                 TO ERR-LOCAL.

           EXEC CICS RECEIVE
               MAP      ('BVVE781')
               MAPSET   ('BVVE781')
               INTO     (BVVE781I)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS AND 36)
               MOVE '0180'             TO ERR-LOCAL
               PERFORM 9400-ERRO-CICS
               MOVE 'VVKK - ERRO AO RECEBER TELA'
                                       TO WRK-MSG-ERRO
               PERFORM 5000-ENCERRAR-TRANSACAO
           END-IF.

           MOVE SPACES                 TO MENSAGO.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    DESCONECTA                                                  *
      *----------------------------------------------------------------*
       4100-TRATAR-PF2                 SECTION.
      *----------------------------------------------------------------*

           MOVE '0190'                 TO ERR-LOCAL.

           EXEC CICS ISSUE
               DISCONNECT
           END-EXEC.

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VOLTA A TELA ANTERIOR                                       *
      *----------------------------------------------------------------*
       4200-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*

           MOVE '0220'                 TO ERR-LOCAL.

           MOVE WRK-COMM-CFUNC-BDSCO   TO WRK-3250-CFUNC-BDSCO.

           EXEC CICS XCTL
               PROGRAM  (WRK-BVVE3250)
               COMMAREA (WRK-AREA-BVVE3250)
               LENGTH   (LENGTH OF WRK-AREA-BVVE3250)
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VOLTA AO MENU DE ROTINAS                                    *
      *----------------------------------------------------------------*
       4300-TRATAR-PF5                 SECTION.
      *----------------------------------------------------------------*

           MOVE '0210'                 TO ERR-LOCAL.

           SET   WRK-MENU-SENHAS       TO  TRUE.
           MOVE  'BVVE'                TO  WRK-SISTEMA.
           MOVE  1                     TO  WRK-VERSAO.

           EXEC CICS XCTL
                PROGRAM  (WRK-SENH1005)
                COMMAREA (WRK-AREA-SENH1005)
                LENGTH   (LENGTH OF WRK-AREA-SENH1005)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

           IF  WRK-ENCERRA
               EXEC CICS
                   RETURN
               END-EXEC
           ELSE
               PERFORM 3000-ENVIAR-TELA
           END-IF.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       4303-TRATAR-PF9                 SECTION.
      *----------------------------------------------------------------*

           MOVE '0220'                 TO ERR-LOCAL.

           PERFORM 3200-MONTAR-FASE-1.

           PERFORM 3000-ENVIAR-TELA.

      *----------------------------------------------------------------*
       4303-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VOLTA AO MENU DO BVVE                                       *
      *----------------------------------------------------------------*
       4305-TRATAR-PF10                SECTION.
      *----------------------------------------------------------------*

           MOVE '0230'                 TO ERR-LOCAL.

           SET   WRK-MENU-BVVE         TO  TRUE.
           MOVE  'BVVE'                TO  WRK-SISTEMA.
           MOVE  1                     TO  WRK-VERSAO.

           EXEC CICS XCTL
               PROGRAM  (WRK-SENH1005)
               COMMAREA (WRK-AREA-SENH1005)
               LENGTH   (LENGTH OF WRK-AREA-SENH1005)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

           IF  WRK-ENCERRA
               EXEC CICS
                   RETURN
               END-EXEC
           ELSE
               PERFORM 3000-ENVIAR-TELA
           END-IF.

      *----------------------------------------------------------------*
       4305-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

STATUS**----------------------------------------------------------------
.     *    LISTA DE SITUACOES POSSIVEIS DA MENSAGEM                    *
.     *----------------------------------------------------------------*
.     *4310-TRATAR-PF11                SECTION.
.     *----------------------------------------------------------------*
.     *
.     *   -MOVE '0240'                 TO ERR-LOCAL.
.     *
.     *   -MOVE WRK-PROGRAMA           TO BVVEW785-CHAMADOR.
.     *   -MOVE WRK-COMM-CFUNC-BDSCO   TO BVVEW785-CFUNC-BDSCO.
.     *   -MOVE WRK-PROGRAMA           TO BVVEW785-EIBTRMID.
.     *   -MOVE WRK-COMMAREA           TO BVVEW785-AREA-CHAMADOR.
.     *   -MOVE BVVE781O               TO BVVEW785-MAPA-CHAMADOR.
.     *
.     *   -EXEC CICS XCTL
.     *        PROGRAM  (WRK-BVVE3785)
.     *        COMMAREA (WRK-AREA-BVVE3785)
.     *        LENGTH   (LENGTH OF WRK-AREA-BVVE3785)
.     *        NOHANDLE
.     *    END-EXEC.
.     *
.     *   -IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
.     *       -MOVE WRK-BVVE3785       TO BVVEWERR-CICS-ACESSADO
.     *       -PERFORM 9400-ERRO-CICS
.     *    END-IF.
.     *
.     *   -EXEC CICS
.     *       -RETURN
.     *    END-EXEC.
.     *
.     *----------------------------------------------------------------*
.     *4310-99-FIM.                    EXIT.
STATUS**---------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    TRATA E N T E R                                             *
      *----------------------------------------------------------------*
       4400-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE '0250'                 TO ERR-LOCAL.

           EVALUATE WRK-COMM-FASE
               WHEN 1
                   MOVE DIAINII        TO WRK-DATA-CALE-DIA-I
                   MOVE MESINII        TO WRK-DATA-CALE-MES-I
                   MOVE ANOINII        TO WRK-DATA-CALE-ANO-I

                   IF (WRK-DATA-CALE-INI   EQUAL LOW-VALUES OR SPACES)
                       PERFORM 3100-OBTER-DATA-HORA
                       MOVE WRK-DATA(1:2)
                                       TO WRK-DATA-CALE-DIA-I
                                          DIAINII
                       MOVE WRK-DATA(4:2)
                                       TO WRK-DATA-CALE-MES-I
                                          MESINII
                       MOVE WRK-DATA(7:4)
                                       TO WRK-DATA-CALE-ANO-I
                                          ANOINII
                   ELSE
                       SET WRK-FLG-DT-INI  TO TRUE
                       PERFORM 4510-INFO-DATA-CALE
                       PERFORM 4520-ACESSAR-CALE
                   END-IF

                   IF (WRK-DATA-CALE-FIN   EQUAL LOW-VALUES OR SPACES)
                       MOVE WRK-DATA-CALE-INI
                                       TO WRK-DATA-CALE-FIN
                       PERFORM 3300-MONTAR-FASE-2
                       PERFORM 2000-CONSISTIR-CAMPOS
                       PERFORM 2900-MONTAR-LISTA
                   ELSE
                       EVALUATE TRUE
                           WHEN (WRK-DATA-CALE-FIN
                                               LESS WRK-DATA-CALE-INI)
                               MOVE WRK-MSG011
                                       TO MENSAGO
                               MOVE -1 TO DIAINIL
                           WHEN (WRK-DATA-CALE-FIN
                                               EQUAL WRK-DATA-CALE-INI)
                               PERFORM 3300-MONTAR-FASE-2
                           WHEN (WRK-DATA-CALE-FIN
                                           GREATER WRK-DATA-CALE-INI)
                               SET WRK-FLG-DT-FIN  TO TRUE
                               PERFORM 4510-INFO-DATA-CALE
                               PERFORM 4520-ACESSAR-CALE
                               PERFORM 3300-MONTAR-FASE-2
                               PERFORM 2000-CONSISTIR-CAMPOS
                               PERFORM 2900-MONTAR-LISTA
                       END-EVALUATE
                   END-IF

               WHEN 2
                   PERFORM 2000-CONSISTIR-CAMPOS
                   PERFORM 2900-MONTAR-LISTA
           END-EVALUATE.

           PERFORM 3000-ENVIAR-TELA.

      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TRATA PF INVALIDA                                           *
      *----------------------------------------------------------------*
       4500-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE '0260'                 TO ERR-LOCAL.

           MOVE  'PF INVALIDA'         TO MENSAGO.

           PERFORM 3000-ENVIAR-TELA.

      *----------------------------------------------------------------*
       4500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4510-INFO-DATA-CALE             SECTION.
      *----------------------------------------------------------------*

           MOVE '0270'                 TO ERR-LOCAL.

           INITIALIZE CALE01-REGISTRO.

           MOVE 'CALE0001'             TO CALE01-ID-BLOCO.
           MOVE 1182                   TO CALE01-TAM-BLOCO.
           MOVE 'F2'                   TO CALE01-FUNCAO.
           MOVE 'SF2001'               TO CALE01-SUB-FUNCAO.
           MOVE 003                    TO CALE01-FORMATO-ARGUMENTO-INI.

           IF (WRK-FLG-DT-INI)
               MOVE WRK-DATA-CALE-INI  TO WRK-AUX-DATA-X-8
           ELSE
               MOVE WRK-DATA-CALE-FIN  TO WRK-AUX-DATA-X-8
           END-IF.

           MOVE WRK-AUX-DATA-X-8       TO CALE01-VLR-ARGUMENTO-INI.

      *----------------------------------------------------------------*
       4510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4520-ACESSAR-CALE               SECTION.
      *----------------------------------------------------------------*

           MOVE '0280'                 TO ERR-LOCAL.

           EXEC CICS LINK
               PROGRAM (WRK-CALE1000)
               COMMAREA (CALE01-REGISTRO)
               LENGTH (LENGTH OF CALE01-REGISTRO)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL)) OR
              (CALE01-COD-RETORNO      NOT EQUAL ZEROS AND 08)
               MOVE WRK-CALE1000       TO BVVEWERR-CICS-ACESSADO
               MOVE '0290'             TO ERR-LOCAL
               PERFORM 9400-ERRO-CICS
           END-IF.

           IF (CALE01-CONSIST-DT-HORA  NOT EQUAL 'C')
               IF (WRK-FLG-DT-INI)
                   MOVE -1             TO DIAINIL
               END-IF
               MOVE WRK-MSG002         TO MENSAGO
               PERFORM 3000-ENVIAR-TELA
           END-IF.

      *----------------------------------------------------------------*
       4520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ENCERRAR TRANSACAO                              *
      *----------------------------------------------------------------*
       5000-ENCERRAR-TRANSACAO         SECTION.
      *----------------------------------------------------------------*

           MOVE '0300'                 TO ERR-LOCAL.

           EXEC CICS SEND
               FROM (WRK-MSG-ERRO)
               ERASE
           END-EXEC.

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA GRAVAR LOG DE ERROS                                *
      *----------------------------------------------------------------*
       9000-GERAR-LOG-ERRO             SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE WRK-PROGRAMA           TO ERR-PGM.
           MOVE DFHEIBLK               TO ERR-DFHEIBLK.
           MOVE SPACES                 TO ERR-SEGM.

           EXEC CICS ASSIGN
                 APPLID      (ERR-CICS)
           END-EXEC.

           EXEC CICS START
                 TRANSID     ('CD11')
                 FROM        (ERRO-AREA)
                 LENGTH      (LENGTH OF ERRO-AREA)
                 NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO DB2                             *
      *----------------------------------------------------------------*
       9300-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*

           MOVE 'DB2'                  TO ERR-TIPO-ACESSO.
           MOVE WRK-PROGRAMA           TO BVVEWERR-DB2-MODULO.
           MOVE SQLCODE                TO BVVEWERR-DB2-SQLCODE.
           MOVE ERR-LOCAL              TO BVVEWERR-DB2-LOCAL.
           MOVE BVVEWERR-DB2-TABELA    TO ERR-DBD-TAB.
           MOVE BVVEWERR-DB2-COMANDO   TO ERR-FUN-COMANDO.
           MOVE SQLCODE                TO ERR-SQL-CODE.
           MOVE SQLCA                  TO ERR-SQLCA.
           MOVE BVVEWERR-ERR-DB2       TO ERR-TEXTO-MSG.

           PERFORM 9000-GERAR-LOG-ERRO.

      *----------------------------------------------------------------*
       9300-DB2-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE WRK-PROGRAMA           TO BVVEWERR-CICS-MODULO.
           MOVE EIBRESP                TO BVVEWERR-CICS-EIBRESP.
           MOVE EIBRESP2               TO BVVEWERR-CICS-EIBRESP2.
           MOVE ERR-LOCAL              TO BVVEWERR-CICS-LOCAL.

           MOVE BVVEWERR-ERR-CICS      TO ERR-TEXTO-MSG.

           MOVE EIBTRMID               TO ERR-TERMINAL.

           PERFORM 9000-GERAR-LOG-ERRO.

      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
