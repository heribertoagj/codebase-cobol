      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1154.
       AUTHOR.     GERSON OTAVIO DE SOUZA.
      *================================================================*
      *              C P M  B R A X I S  -  S I S T E M A S            *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1154                                     *
      *    PROGRAMADOR  : GERSON OTAVIO DE SOUZA   - CPM BRAXIS/FPOLIS *
      *    ANALISTA CPM : MAURICIO NAPOLEAO        - CPM BRAXIS/FPOLIS *
      *    ANALISTA     : WAGNER  SILVA - PROCWORK - GRUPO 50          *
      *    DATA         : 24/09/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      EMITIR RELATORIOS DE EVENTOS.                             *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                INCLUDE/BOOK        *
      *      ARQCOBR2                                I#GFCTMW          *
      *      AVIREPIQ                                I#GFCT39          *
      *      FIMCOBRA                                I#GFCT39          *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.PARM_SERVC_TARIF                GFCTB0A2          *
NLC09 *       DB2PRD.MOVTO_EVNTO_CRRTT               GFCTB092          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100 - ROTINA PARA CANCELAMENTO DE PROGRAMAS          *
      *      BRAD7600 - ROTINA PARA OBTER DATA E HORA DO SISTEMA       *
      *      BRAD1050 - CONEXAO DO COBOL COM O DB2                     *
      *                                                                *
071031*================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : WAGNER  SILVA - PROCWORK - GRUPO 50          *
      *    DATA         : 31/10/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      INCLUIR AGENCIA E CONTA DE DEBITO NOS ARQUIVOS DE SAIDA   *
      *----------------------------------------------------------------*
      *                       A L T E R A C A O                        *
NLC09 *    ANALISTA     : NELSON CORREIA- PROCWORK - GRUPO 50          *
  .   *    DATA         : 34/08/2010                                   *
  .   *                                                                *
  .   *    OBJETIVO     :                                              *
  .   *      INCLUIR ACESSO A TABELA GFCTB092                          *
  .   *                                                                *
071031*================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR  :  CIBELE               - SONDA PROCWORK       *
      *    ANALISTA     :  HELENA               - DDS                  *
      *    DATA         :  25/11/2010                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      RECOMPILACAO A PEDIDO DO ANALISTA.                        *
      *                                                                *
      *================================================================*
GP436 *----------------------------------------------------------------*
  "   * ALTERACOES:                                                    *
  "   * ANALISTA RESPONSAVEL : EDUARDO HENRIQUE FRANCA                 *
  "   *                                                                *
  "   * DATA       NOME     DESCRICAO                                  *
  "   * ========== ======== ===========================================*
GP436 * 02/10/2021 YOTOFUJI GP436 - IN6191986 - ABEND SQLCODE -811     *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQCOBR2 ASSIGN      TO UT-S-ARQCOBR2
                      FILE STATUS      IS WRK-FS-ARQCOBR2.

           SELECT AVIREPIQ ASSIGN      TO UT-S-AVIREPIQ
                      FILE STATUS      IS WRK-FS-AVIREPIQ.

           SELECT FIMCOBRA ASSIGN      TO UT-S-FIMCOBRA
                      FILE STATUS      IS WRK-FS-FIMCOBRA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  REGISTROS QUE PASSARAM PELO PROCESSO DE COBRANCA  *
      *              ORG. SEQUENCIAL   -   LRECL = 340                 *
      *----------------------------------------------------------------*

       FD  ARQCOBR2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQCOBR2                 PIC  X(340).

      *----------------------------------------------------------------*
      *    OUTPUT :  REGISTROS COBRADOS                                *
      *              ORG. SEQUENCIAL   -   LRECL = 070                 *
      *----------------------------------------------------------------*

       FD  AVIREPIQ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-AVIREPIQ                 PIC  X(070).

      *----------------------------------------------------------------*
      *    OUTPUT :  REGISTROS COBRADOS                                *
      *              ORG. SEQUENCIAL   -   LRECL = 070                 *
      *----------------------------------------------------------------*

       FD  FIMCOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-FIMCOBRA                 PIC  X(070).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING GFCT1154  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-ARQCOBR2      PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-AVIREPIQ      PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-FIMCOBRA      PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-DESPR-ARQCOBR2      PIC  9(11)  COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC  X(08)          VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC  ZZZZZZZZZ99
                                                           VALUE ZEROS.
           05  WRK-DT-CORRENTE         PIC  X(10)          VALUE SPACES.
           05  WRK-DT-EVNTO-TARIF      PIC  X(10)          VALUE SPACES.
           05  WRK-TENTV-COBR          PIC  X(01)          VALUE SPACES.
           05  WRK-QTENTV-SEM-COBR     PIC  9(03)          VALUE ZEROS.
           05  WRK-AUX-05              PIC  9(05)          VALUE ZEROS.
           05  FILLER REDEFINES WRK-AUX-05.
             10 FILLER                 PIC  9(02).
             10 WRK-AUX-05-R           PIC  9(03).
           05  WRK-S9-03               PIC +9(03)          VALUE ZEROS.
           05  FILLER REDEFINES WRK-S9-03.
             10 FILLER                 PIC  X(01).
             10 WRK-Z09-03             PIC  9(03).
           05  WRK-TESTE               PIC  9(03)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*   VARIAVEIS DE FILE-STATUS   *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FILE-STATUS.
           05  WRK-FS-ARQCOBR2         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-FIMCOBRA         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-AVIREPIQ         PIC  X(02)          VALUE SPACES.
           05  WRK-ABERTURA            PIC  X(13)          VALUE
               ' NA ABERTURA'.
           05  WRK-LEITURA             PIC  X(13)          VALUE
               'NA LEITURA'.
           05  WRK-GRAVACAO            PIC  X(13)          VALUE
               ' NA GRAVACAO'.
           05  WRK-FECHAMENTO          PIC  X(13)          VALUE
               'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       MENSAGENS DE ERRO      *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ERRO.
           05  FILLER                  PIC  X(05)          VALUE SPACES.
           05  FILLER                  PIC  X(07)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           05  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           05  WRK-ARQUIVO             PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(17)          VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(11)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  AREA DAS CHAVES ARQUIVOS    *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-CHAVES.
           05  WRK-CHAVE-ATUAL         PIC  9(13)          VALUE ZEROS.
           05  FILLER REDEFINES WRK-CHAVE-ATUAL.
             10 WRK-TARIFA-ORIG-ATU    PIC  9(05).
             10 WRK-DT-EVNT-ORIG-ATU   PIC  9(08).
           05  WRK-CHAVE-ANT           PIC  9(13)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  AREA DA BRAD7600            *'.
      *----------------------------------------------------------------*

       01  WRK-7600-DATA-HORA.
           05  WRK-7600-DT-JULIANA     PIC 9(005) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-AAMMDDD     PIC 9(007) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-AAAAMMDD    PIC 9(009) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-HHMMSS      PIC 9(007) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-HHMMSSSS    PIC 9(013) COMP-3   VALUE ZEROS.
           05  WRK-7600-TIMESTAMP.
               10  WRK-7600-ANO        PIC 9(004)          VALUE ZEROS.
               10  WRK-7600-MES        PIC 9(002)          VALUE ZEROS.
               10  WRK-7600-DIA        PIC 9(002)          VALUE ZEROS.
               10  WRK-7600-HORA       PIC 9(002)          VALUE ZEROS.
               10  WRK-7600-MINUTOS    PIC 9(002)          VALUE ZEROS.
               10  WRK-7600-SEGUNDOS   PIC 9(002)          VALUE ZEROS.
               10  WRK-7600-MICROSEG   PIC 9(006)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA DO ARQCOBR2         *'.
      *----------------------------------------------------------------*

       COPY I#GFCTMW.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA DO AVIREPIQ         *'.
      *----------------------------------------------------------------*

       COPY I#GFCT39.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*      AREA PARA BRAD7100      *'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA PARA TABELA DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*   FIM DA WORKING GFCT1154    *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR.

           PERFORM 0600-VERIFICAR-VAZIO.

           PERFORM 0800-PROCESSAR      UNTIL
                   WRK-FS-ARQCOBR2     EQUAL '10'.

           PERFORM 1300-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIALIZACOES DO PROGRAMA                           *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ARQCOBR2
                OUTPUT AVIREPIQ
                       FIMCOBRA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           INITIALIZE ACU-ACUMULADORES
                      FD-ARQCOBR2
                      WRK-7600-DATA-HORA.

           CALL 'BRAD7600'             USING WRK-7600-DATA-HORA.

           STRING WRK-7600-DIA  '.'
                  WRK-7600-MES  '.'
                  WRK-7600-ANO
           DELIMITED BY SIZE           INTO WRK-DT-CORRENTE.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE CHAMADA PARA TESTE DE FILE-STATUS                 *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-ARQCOBR2.

           PERFORM 0400-TESTAR-FS-AVIREPIQ.

           PERFORM 0500-TESTAR-FS-FIMCOBRA.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE TESTE DE FILE-STATUS ARQUIVO ARQCOBR2             *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ARQCOBR2         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQCOBR2         NOT EQUAL '00'
               MOVE 'ARQCOBR2'         TO WRK-ARQUIVO
               MOVE WRK-FS-ARQCOBR2    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE TESTE DE FILE-STATUS ARQUIVO AVIREPIQ             *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-AVIREPIQ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AVIREPIQ         NOT EQUAL '00'
               MOVE 'AVIREPIQ'         TO WRK-ARQUIVO
               MOVE WRK-FS-AVIREPIQ    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE TESTE DE FILE-STATUS ARQUIVO FIMCOBRA             *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-FIMCOBRA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FIMCOBRA         NOT EQUAL '00'
               MOVE 'FIMCOBRA'         TO WRK-ARQUIVO
               MOVE WRK-FS-FIMCOBRA    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICACAO DE ARQUIVO  VAZIO                   *
      *----------------------------------------------------------------*
       0600-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0700-LER-ARQCOBR2.

           IF  WRK-FS-ARQCOBR2         EQUAL '10'
               DISPLAY '********** GFCT1154 **********'
               DISPLAY '*                            *'
               DISPLAY '*   ARQUIVO ARQCOBR2 VAZIO   *'
               DISPLAY '*  PROCESSAMENTO  ENCERRADO  *'
               DISPLAY '*                            *'
               DISPLAY '********** GFCT1154 **********'
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO ARQCOBR2                       *
      *----------------------------------------------------------------*
       0700-LER-ARQCOBR2               SECTION.
      *----------------------------------------------------------------*

           READ ARQCOBR2               INTO REG-GFCTMW.

           IF  WRK-FS-ARQCOBR2         EQUAL '10'
               GO                      TO 0700-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQCOBR2.


           MOVE GFCTMW-TARIFA-ORIG     TO WRK-TARIFA-ORIG-ATU.
           MOVE GFCTMW-DATA-EVNTO-ORIG TO WRK-DT-EVNT-ORIG-ATU.

           ADD 1                       TO ACU-LIDOS-ARQCOBR2.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSAMENTO                                     *
      *----------------------------------------------------------------*
       0800-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS          TO      WRK-TESTE.

           IF WRK-CHAVE-ATUAL          NOT EQUAL WRK-CHAVE-ANT
              PERFORM 0900-BUSCAR-DADOS-TARIFA
           END-IF.

           IF (GFCTMW-TENTV-DEB-ATU    EQUAL GFCTMW-TENTV-DEB-MAX  OR
               GFCTMW-VALOR-LIQ92      EQUAL GFCTMW-VALOR-COBR92)
               PERFORM 1000-FORMATAR-REG-SAIDA
               PERFORM 1100-GRAVAR-FIMCOBRA
           ELSE
               IF  WRK-TENTV-COBR      EQUAL 'S'                   AND
                   WRK-QTENTV-SEM-COBR EQUAL GFCTMW-TENTV-DEB-ATU
                   PERFORM 1000-FORMATAR-REG-SAIDA
                   PERFORM 1200-GRAVAR-AVIREPIQ
               ELSE
                   ADD 1               TO ACU-DESPR-ARQCOBR2
               END-IF
           END-IF.

           PERFORM 0700-LER-ARQCOBR2.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER OD DADOS DA TARIFA NA TABLE GFCTB0A2      *
      *----------------------------------------------------------------*
       0900-BUSCAR-DADOS-TARIFA        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTMW-TARIFA-ORIG     TO CSERVC-TARIF     OF GFCTB0A2.

           STRING  GFCTMW-DATA-EVNTO-ORIG(7:2) '.'
                   GFCTMW-DATA-EVNTO-ORIG(5:2) '.'
                   GFCTMW-DATA-EVNTO-ORIG(1:4)
           DELIMITED SIZE              INTO WRK-DT-EVNTO-TARIF.

           EXEC SQL
             SELECT
                   CINDCD_TENTV_COBR,
                   QTENTV_SEM_COBR
             INTO
                   :GFCTB0A2.CINDCD-TENTV-COBR,
                   :GFCTB0A2.QTENTV-SEM-COBR
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF       AND
                   DINIC_VGCIA_TARIF <= :WRK-DT-EVNTO-TARIF          AND
                   DFIM_VGCIA_TARIF  >= :WRK-DT-EVNTO-TARIF
GP436************* PARA TRATAR SQLCODE -811 - IN6191986
GP436              ORDER  BY HIDTFD_MANUT_SERVC DESC
GP436              FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CINDCD-TENTV-COBR  OF GFCTB0A2
                                       TO WRK-TENTV-COBR
               MOVE QTENTV-SEM-COBR    OF GFCTB0A2
                                       TO WRK-S9-03
               MOVE WRK-Z09-03         TO WRK-QTENTV-SEM-COBR
           ELSE
               MOVE  'N'               TO WRK-TENTV-COBR
               MOVE  ZEROS             TO WRK-QTENTV-SEM-COBR
           END-IF.

           MOVE WRK-CHAVE-ATUAL        TO WRK-CHAVE-ANT.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR AREA DE SAIDA                          *
      *----------------------------------------------------------------*
       1000-FORMATAR-REG-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO REG-GFCT39.

           INITIALIZE REG-GFCT39.

071031     MOVE GFCTMW-AGENCIA-DEB     TO GFCT39-AGENCIA-DEB.
071031     MOVE GFCTMW-CONTA-DEB       TO GFCT39-CONTA-DEB.
           MOVE GFCTMW-ROTINA-ORIG     TO GFCT39-ROTINA-ORIG.
           MOVE GFCTMW-DATA-ORIG       TO GFCT39-DATA-ORIG.
           MOVE GFCTMW-NUM-ARQ-ORIG    TO GFCT39-NUM-ARQ-ORIG.
           MOVE GFCTMW-SEQ-ARQ-ORIG    TO GFCT39-SEQ-ARQ-ORIG.
           MOVE GFCTMW-TARIFA-ORIG     TO GFCT39-TARIFA-ORIG.
           MOVE GFCTMW-VALOR-LIQ92     TO GFCT39-VALOR-LIQUIDO.
           MOVE GFCTMW-VALOR-COBR92    TO GFCT39-VALOR-COBRADO.
           MOVE WRK-DT-CORRENTE        TO GFCT39-DATA-DEBITO.
           MOVE GFCTMW-TENTV-DEB-ATU   TO GFCT39-TENTV-DEB-ATU.
           MOVE GFCTMW-TENTV-DEB-MAX   TO GFCT39-TENTV-DEB-MAX.
           MOVE WRK-QTENTV-SEM-COBR    TO WRK-AUX-05-R.
           MOVE WRK-AUX-05             TO GFCT39-AVISO.

            PERFORM  1050-ACESSA-GFCTB092.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR DADOS DA TABELA GFCTB092                *
      *----------------------------------------------------------------*
       1050-ACESSA-GFCTB092            SECTION.
      *----------------------------------------------------------------*
           MOVE GFCTMW-DATA-ORIG       TO DENVIO-MOVTO-TARIF OF GFCTB092
           MOVE GFCTMW-ROTINA-ORIG     TO CROTNA-ORIGE-MOVTO OF GFCTB092
           MOVE GFCTMW-NUM-ARQ-ORIG    TO CNRO-ARQ-MOVTO     OF GFCTB092
           MOVE GFCTMW-SEQ-ARQ-ORIG    TO CSEQ-MOVTO         OF GFCTB092

           EXEC SQL
            SELECT
                 CDOCTO_EXTRT
            INTO
                :GFCTB092.CDOCTO-EXTRT
             FROM
                  DB2PRD.MOVTO_EVNTO_CRRTT
             WHERE
                   DENVIO_MOVTO_TARIF =:GFCTB092.DENVIO-MOVTO-TARIF
             AND   CROTNA_ORIGE_MOVTO =:GFCTB092.CROTNA-ORIGE-MOVTO
             AND   CNRO_ARQ_MOVTO     =:GFCTB092.CNRO-ARQ-MOVTO
             AND   CSEQ_MOVTO         =:GFCTB092.CSEQ-MOVTO
            END-EXEC.
            IF (SQLCODE                 NOT EQUAL ZEROS) OR
               (SQLWARN0                EQUAL 'W')
                MOVE 'DB2'              TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
                MOVE  SQLCODE           TO ERR-SQL-CODE
                MOVE 'SELECT'           TO ERR-FUN-COMANDO
                MOVE  SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'             TO ERR-LOCAL
                PERFORM 9999-ROTINA-ERRO
            END-IF.

            IF  SQLCODE                     EQUAL ZEROS
                MOVE CDOCTO-EXTRT OF GFCTB092  TO GFCT39-CDOCTO-EXTRT
             ELSE
                MOVE ZEROS                     TO GFCT39-CDOCTO-EXTRT
            END-IF.

      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVACAO DO ARQUIVO FIMCOBRA                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-FIMCOBRA            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-FIMCOBRA           FROM REG-GFCT39.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-FIMCOBRA.

           ADD 1                       TO ACU-GRAVA-FIMCOBRA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVACAO DO ARQUIVO AVIREPIQ                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-AVIREPIQ            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-AVIREPIQ           FROM REG-GFCT39.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-AVIREPIQ.

           ADD 1                       TO ACU-GRAVA-AVIREPIQ.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA                           *
      *----------------------------------------------------------------*
       1300-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-TOTAIS.

           CLOSE ARQCOBR2
                 AVIREPIQ
                 FIMCOBRA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMITIR AS ESTATISTICAS                          *
      *----------------------------------------------------------------*
       1400-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT1154 '
                   '*********************'.
           DISPLAY '*                               '
                   '                  *'.
           DISPLAY '*                               '
                   '                  *'.

           MOVE ACU-LIDOS-ARQCOBR2     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM ARQCOBR2 ......: '
                   WRK-MASCARA ' *'

           MOVE ACU-GRAVA-AVIREPIQ     TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO AVIREPIQ....: '
                   WRK-MASCARA ' *'

           MOVE ACU-GRAVA-FIMCOBRA     TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO FIMCOBRA....: '
                   WRK-MASCARA ' *'

           MOVE ACU-DESPR-ARQCOBR2     TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO DESPREZADOS.: '
                   WRK-MASCARA ' *'
           DISPLAY '*                              '
                   '                   *'.
           DISPLAY '******************** GFCT1154 '
                   '*********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE ERRO                                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1154'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
