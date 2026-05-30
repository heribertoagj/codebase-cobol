      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5538.
       AUTHOR.     SIMONI FAVRETTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5538                                    *
      *    PROGRAMADORA:   SIMONI FAVRETTO         - CPM/PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM/PATO BRANCO   *
      *    ANALISTA....:   RICARDO PEREIRA         - PROCKWORK / GP.50 *
      *    DATA........:   14/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   VERIFICAR SE EXISTE UNIFICACAO POR CPF/CNPJ.*
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.TDEB_UNFCA_CLI       GFCTB0H8        *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2.                                 *
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

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREAS AULIXIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(070)         VALUE
           'PROGRAMA CHAMADOR NAO INFORMADO'.
       77  WRK-MSG002                  PIC  X(070)         VALUE
           'AMBIENTE DIFERENTE DE B/O'.
       77  WRK-MSG003                  PIC  X(070)         VALUE
           'PARAMETROS DE PESQUISA INVALIDOS'.
       77  WRK-MSG004                  PIC  X(070)         VALUE
           'ERRO NO MODULO GFCT5538 - ACESSO DB2 GFCTB0H8'.
       77  WRK-MSG005                  PIC  X(070)         VALUE
           'CHAVE NAO CADASTRADA'.
       77  WRK-MSG006                  PIC  X(070)         VALUE
           'PROCESSAMENTO OK'.

       01  WRK-QTDE-REGISTROS          PIC S9(009) COMP    VALUE ZEROS.

       01  WRK-AREA-MENSAGENS.
           05  WRK-MSG-0M1.
             10  WRK-0M1-DESCRICAO     PIC X(025)          VALUE
                 'ERRO NO ACESSO AO MODULO '.
             10  WRK-0M1-MODULO        PIC X(008)          VALUE SPACES.
             10  FILLER                PIC X(001)          VALUE SPACES.
             10  FILLER                PIC X(008)          VALUE
                 'COD RET='.
             10  WRK-0M1-RET-CHAMADO   PIC 9(003)          VALUE ZEROS.
             10  FILLER                PIC X(005)          VALUE
                 ' SQL='.
             10  WRK-0M1-SQL-CHAMADO   PIC 9(003)          VALUE ZEROS.
             10 FILLER                 PIC X(006)          VALUE SPACES.
           05  WRK-MSG-0M2.
             10  WRK-0M2-DESCRICAO     PIC X(050)          VALUE
                 'AMBIENTE DIFERENTE DE B/O - AMBIENTE INFORMADO = '.
             10  WRK-0M2-AMBIENTE      PIC X(001)          VALUE SPACES.
             10  FILLER                PIC X(024)          VALUE SPACES.
           05  WRK-MSG-0M3.
             10  WRK-0M3-DESCRICAO     PIC X(075)          VALUE
                 'PROGRAMA CHAMADOR NAO INFORMADO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA IMAGEM INF I#GFCT0M *'.
      *----------------------------------------------------------------*

       01  WRK-0M-AREA-ERROS.
           05  WRK-0M-TRANSACAO         PIC X(008)         VALUE SPACES.
           05  WRK-0M-ERRO-AREAS.
               10  WRK-0M-AREA-COMUM.
                 15 WRK-0M-TIPO-ACESSO  PIC X(003)         VALUE SPACES.
                 15 WRK-0M-PROGRAMA     PIC X(008)         VALUE SPACES.
               10  WRK-0M-APLICACAO.
                 15 WRK-0M-TEXTO        PIC X(075)         VALUE SPACES.
               10  WRK-0M-ERRO-SQL      REDEFINES WRK-0M-APLICACAO.
                 15  WRK-0M-NOME-TAB    PIC X(018).
                 15  WRK-0M-COMANDO-SQL PIC X(010).
                 15  WRK-0M-STACODE     PIC X(004).
                 15  WRK-0M-SQLCODE REDEFINES
                     WRK-0M-STACODE     PIC S9(009) COMP-4.
                 15  WRK-0M-LOCAL       PIC X(004).
                 15  WRK-0M-SEGM        PIC X(008).
                 15  FILLER             PIC X(031).
           05  WRK-0M-ERRO-SQLCA.
             10  WRK-0M-SQLCA-AREA      PIC X(136)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-5538-AREA-ENTRADA.
           05  LNK-5538-AMBIENTE       PIC  X(001).
           05  LNK-5538-CHAMADOR       PIC  X(008).
           05  LNK-5538-CCGC-CPF       PIC  9(009).
           05  LNK-5538-CFLIAL-CGC     PIC  9(005).
           05  LNK-5538-CCTRL-CPF-CGC  PIC  9(002).
           05  LNK-5538-CSERVC-TARIF   PIC  9(005).

       01  LNK-5538-AREA-SAIDA.
           05  LNK-5538-COD-RETORNO    PIC 9(002).
      *        00 - PROCESSAMENTO OK
      *        01 - AMBIENTE DIFERENTE DE 'B' E 'O'
      *        02 - CHAVE NAO CADASTRADA
      *        03 - PARAMETROS DE PESQUISA INVALIDOS
      *        04 - PROGRAMA CHAMADOR NAO INFORMADO
      *        99 - ERRO DB2
           05  LNK-5538-COD-SQL-ERRO   PIC S9(003).
           05  LNK-5538-COD-MSG-GFCT   PIC  9(004).
           05  LNK-5538-DESC-MSG       PIC  X(070).
           05  LNK-5538-QTD-OCORR      PIC  9(005).
           05  LNK-5538-DADOS-RETORNO.
             10  LNK-5538-UNIFICADO    PIC  X(001).

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING LNK-5538-AREA-ENTRADA
                                             LNK-5538-AREA-SAIDA
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *  EXECUTA AS ROTINAS PRINCIPAIS DO PROGRAMA                     *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  INICIALIZA AREAS DE RETORNO AO CHAMADOR                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO LNK-5538-AREA-SAIDA
                                          WRK-0M-AREA-ERROS.

           INITIALIZE LNK-5538-AREA-SAIDA
                      WRK-0M-AREA-ERROS.

           PERFORM 1100-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  CONSISTE DADOS DE ENTRADA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           MOVE LNK-5538-CHAMADOR      TO WRK-0M-TRANSACAO.
           MOVE 'GFCT5538'             TO WRK-0M-PROGRAMA.

           IF  LNK-5538-CHAMADOR       EQUAL SPACES      OR
               LNK-5538-CHAMADOR       EQUAL LOW-VALUES  OR
               LNK-5538-CHAMADOR       EQUAL HIGH-VALUES
               MOVE 'APL'              TO WRK-0M-TIPO-ACESSO
               MOVE 04                 TO LNK-5538-COD-RETORNO
               MOVE WRK-MSG001         TO LNK-5538-DESC-MSG
               MOVE ZEROS              TO LNK-5538-COD-SQL-ERRO
                                          LNK-5538-COD-MSG-GFCT
                                          LNK-5538-QTD-OCORR
               MOVE WRK-MSG-0M3        TO WRK-0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  LNK-5538-AMBIENTE       NOT EQUAL 'B' AND 'O'
               MOVE 'APL'              TO WRK-0M-TIPO-ACESSO
               MOVE 01                 TO LNK-5538-COD-RETORNO
               MOVE WRK-MSG002         TO LNK-5538-DESC-MSG
               MOVE ZEROS              TO LNK-5538-COD-SQL-ERRO
                                          LNK-5538-COD-MSG-GFCT
                                          LNK-5538-QTD-OCORR
               MOVE LNK-5538-AMBIENTE  TO WRK-0M2-AMBIENTE
               MOVE WRK-MSG-0M2        TO WRK-0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  LNK-5538-CCGC-CPF       NOT NUMERIC OR
RIC   *        LNK-5538-CCGC-CPF       EQUAL ZEROS OR
               LNK-5538-CFLIAL-CGC     NOT NUMERIC OR
RIC   *        LNK-5538-CFLIAL-CGC     EQUAL ZEROS OR
               LNK-5538-CCTRL-CPF-CGC  NOT NUMERIC OR
RIC   *        LNK-5538-CCTRL-CPF-CGC  EQUAL ZEROS OR
RIC            LNK-5538-CSERVC-TARIF   NOT NUMERIC
RIC   *        LNK-5538-CSERVC-TARIF   EQUAL ZEROS
               MOVE 03                 TO LNK-5538-COD-RETORNO
               MOVE WRK-MSG003         TO LNK-5538-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAMENTO CENTRAL DO PROGRAMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  LNK-5538-AMBIENTE       EQUAL 'B'
               CALL 'POOL1050'
           END-IF.

ST25X6*    MOVE LNK-5538-CCGC-CPF      TO CCGC-CPF         OF GFCTB0H8.
ST25X6*    MOVE LNK-5538-CFLIAL-CGC    TO CFLIAL-CGC       OF GFCTB0H8.
ST25X6*    MOVE LNK-5538-CCTRL-CPF-CGC TO CCTRL-CPF-CGC    OF GFCTB0H8.
ST25X6     MOVE LNK-5538-CCGC-CPF      TO CCGC-CPF-ST      OF GFCTB0H8.
ST25X6     MOVE LNK-5538-CFLIAL-CGC    TO CFLIAL-CGC-ST    OF GFCTB0H8.
ST25X6     MOVE LNK-5538-CCTRL-CPF-CGC TO CCTRL-CPF-CGC-ST OF GFCTB0H8.
           MOVE LNK-5538-CSERVC-TARIF  TO CSERVC-TARIF     OF GFCTB0H8.
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HEXCL-DEB-UNIC   OF GFCTB0H8.

           EXEC SQL
             SELECT COUNT(*)
             INTO :WRK-QTDE-REGISTROS
             FROM   DB2PRD.TDEB_UNFCA_CLI
             WHERE
ST25X6*               CCGC_CPF           = :GFCTB0H8.CCGC-CPF      AND
ST25X6*               CFLIAL_CGC         = :GFCTB0H8.CFLIAL-CGC    AND
ST25X6*               CCTRL_CPF_CGC      = :GFCTB0H8.CCTRL-CPF-CGC AND
ST25X6                CCGC_CPF_ST        = :GFCTB0H8.CCGC-CPF-ST   AND
ST25X6                CFLIAL_CGC_ST      = :GFCTB0H8.CFLIAL-CGC-ST AND
ST25X6                CCTRL_CPF_CGC_ST  = :GFCTB0H8.CCTRL-CPF-CGC-ST AND
                   CSERVC_TARIF       = :GFCTB0H8.CSERVC-TARIF  AND
                   HEXCL_DEB_UNIC     = :GFCTB0H8.HEXCL-DEB-UNIC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT5538'         TO WRK-0M1-MODULO
               MOVE 99                 TO LNK-5538-COD-RETORNO
               MOVE 099                TO WRK-0M1-RET-CHAMADO
               MOVE SQLCODE            TO LNK-5538-COD-SQL-ERRO
                                          WRK-0M1-SQL-CHAMADO
               MOVE ZEROS              TO LNK-5538-COD-MSG-GFCT
               MOVE WRK-MSG004         TO LNK-5538-DESC-MSG
               MOVE ZEROS              TO LNK-5538-QTD-OCORR
               MOVE 'APL'              TO WRK-0M-TIPO-ACESSO
               MOVE WRK-MSG-0M1        TO WRK-0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-QTDE-REGISTROS      EQUAL ZEROS
               MOVE 02                 TO LNK-5538-COD-RETORNO
               MOVE WRK-MSG005         TO LNK-5538-DESC-MSG
               MOVE 'N'                TO LNK-5538-UNIFICADO
           ELSE
               MOVE 'S'                TO LNK-5538-UNIFICADO
               MOVE WRK-MSG006         TO LNK-5538-DESC-MSG
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FINALIZA PROGRAMA E RETORNA AO CHAMADOR                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-0M-AREA-ERROS      TO GFCT0M-AREA-ERROS.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
