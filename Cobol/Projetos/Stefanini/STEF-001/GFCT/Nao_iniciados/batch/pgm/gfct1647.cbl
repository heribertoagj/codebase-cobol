      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1647.
       AUTHOR. JEAN ELIAS.
      *================================================================*
      *                     C P M   S I S T E M A S                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1647                                     *
      *    PROGRAMADOR  : JEAN ELIAS                     - CPM/FPOLIS  *
      *    ANALISTA CPM : ALEXANDRE PEREIRA              - CPM/FPOLIS  *
      *    ANALISTA     : MARIA V. TORQUATO - PROCWORK   - GRUPO 50    *
      *    DATA         : 09/02/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *    - ATUALIZA A TABELA GFCTB092 - DB2.MOVTO_EVNTO_CRRTT DOS    *
      *      REGISTROS AGENDADOS QUE TIVERAM A SUSPENSAO DE TARIFA     *
      *      LIBERADA PARA A COBRANCA EM D+1.                          *
      *                                                                *
      *    - ATUALIZA A TABELA GFCTB0E7 - DB2PRD.SUSP_TARIF_AGPTO      *
      *      REGISTROS SUSPENSOS E LIBERADOS, ALTERANDO O STATUS.      *
      *      PARA 'E'.                                                 *
      *                                                                *
      *    - ATUALIZA A TABELA GFCTB0E8 - DB2PRD.SUSP_TARIF_CTA        *
      *      REGISTROS SUSPENSOS E LIBERADOS, ALTERANDO O STATUS.      *
      *      PARA 'E'.                                                 *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      AGENDADO                              I#GFCTYJ            *
      *      ARQTB0E7                              I#GFCTYK            *
      *      ARQTB0E8                              I#GFCTYI            *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.PARM_DATA_PROC                 GFCTB0A1            *
      *      DB2PRD.MOVTO_EVNTO_CRRTT              GFCTB092            *
      *      DB2PRD.SUSP_TARIF_AGPTO               GFCTB0E7            *
      *      DB2PRD.SUSP_TARIF_CTA                 GFCTB0E8            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - MODULO TRATAMENTO DE ERRO.                     *
      *      CKRS0100 - TRATAMENTO DE COMMIT-RESTART.                  *
      *      GFCT5555 - MODULO PARA UPDATE DE SITUACOES NAS TABELAS *
      *================================================================*
      * 07/12/2007 - INCLUSAO DO MODULO GFCT5555 DE ATUALIZACAO DE     *
      * SITUACAO NAS TABELAS - RICARDO FERRARA                         *
      *================================================================*
CAPG=I*================================================================*
.     *      PROGRAMADOR: PRISCILA BARBOSA- CAPGEMINI                 *
.     *      ANALISTA...: PRISCILA BARBOSA- CAPGEMINI                 *
.     *      DATA.......: 27/07/2015                                  *
.     *---------------------------------------------------------------*
.     *      OBJETIVO: INCLUIR A CHAMADA DO MODULO CKRS1000           *
.     *---------------------------------------------------------------*
.     *      MODULOS.: CKRS1000 - IDENTIFICAR DINAMICAMENTE QUAL O    *
.     *                           TIPO DE CONEXAO ESTA SENDO UTILIZADA*
.     *---------------------------------------------------------------*
.     *   ESTA MANUTENCAO ESTA IDENTIFICADA NO PROGRAMA COMO: CAPG=I  *
CAPG=I*===============================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*
       FILE-CONTROL.

           SELECT AGENDADO ASSIGN      TO UT-S-AGENDADO
                      FILE STATUS      IS WRK-FS-AGENDADO.

           SELECT ARQTB0E7 ASSIGN      TO UT-S-ARQTB0E7
                      FILE STATUS      IS WRK-FS-ARQTB0E7.

           SELECT ARQTB0E8 ASSIGN      TO UT-S-ARQTB0E8
                      FILE STATUS      IS WRK-FS-ARQTB0E8.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*


      *----------------------------------------------------------------*
      *   INPUT:  REGISTROS EVENTO LIBERADO E AGENDADOS                *
      *           ORG.SEQUENCIAL  -  LRECL = 073                       *
      *----------------------------------------------------------------*

       FD  AGENDADO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTYJ.

      *----------------------------------------------------------------*
      *    INPUT:  TARIFAS LIBERADAS POR AGRUPAMENTO   EM D+1          *
      *            ORG. SEQUENTIAL     -  LRECL = 67                   *
      *----------------------------------------------------------------*

       FD  ARQTB0E7
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTYK.

      *----------------------------------------------------------------*
      *    INPUT:  TARIFAS LIBERADAS POR AGENCIA/CONTA EM D+1          *
      *            ORG. SEQUENTIAL     -  LRECL = 41                   *
      *----------------------------------------------------------------*

       FD  ARQTB0E8
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTYI.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*   INICIO DA WORKING GFCT1647 *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC  X(008)         VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC Z.ZZZ.ZZ9       VALUE ZEROS.
           05 WRK-CKRS0100             PIC X(08)           VALUE
              'CKRS0100'.
           05 WRK-AREA-RESTART.
               10  WRK-LIDOS-AGENDADO  PIC 9(07)           VALUE ZEROS.
               10  WRK-ATU-GFCTB092    PIC 9(07)           VALUE ZEROS.
               10  WRK-LIDOS-ARQTB0E7  PIC 9(07)           VALUE ZEROS.
               10  WRK-ATU-GFCTB0E7    PIC 9(07)           VALUE ZEROS.
               10  WRK-LIDOS-ARQTB0E8  PIC 9(07)           VALUE ZEROS.
               10  WRK-ATU-GFCTB0E8    PIC 9(07)           VALUE ZEROS.
           05 WRK-FIM                  PIC X(01)           VALUE SPACES.
           05 WRK-FIM-TB7              PIC X(01)           VALUE SPACES.
           05 WRK-FIM-TB8              PIC X(01)           VALUE SPACES.
           05 WRK-MODULO               PIC X(008)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-AGENDADO       PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-LIDOS-ARQTB0E7       PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-LIDOS-ARQTB0E8       PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-ATU-GFCTB092         PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-ATU-GFCTB0E7         PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-ATU-GFCTB0E8         PIC 9(009) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA TO FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-FS-AGENDADO             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQTB0E7             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQTB0E8             PIC  X(002)         VALUE SPACES.
       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01 WRK-LEITURA                  PIC  X(013)         VALUE
           ' NA  LEITURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(007)          VALUE SPACES.
           05  FILLER                  PIC X(007)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(013)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(017)          VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE ' *'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* AREA PARA GFCT5555 *'.
      *---------------------------------------------------------------*

       01  WRK-5555-AREA-ENTRADA.
           05  WRK-5555-CHAMADOR       PIC  X(008)         VALUE SPACES.
           05  WRK-5555-DENVIO-MOVTO   PIC  X(010)         VALUE SPACES.
           05  WRK-5555-CROTNA-ORIGE   PIC  X(004)         VALUE SPACES.
           05  WRK-5555-CNRO-ARQ       PIC  9(002)         VALUE ZEROS.
           05  WRK-5555-CSEQ-MOVTO     PIC  9(011)         VALUE ZEROS.
           05  WRK-5555-CSIT-EVNTO     PIC  9(002)         VALUE ZEROS.
           05  WRK-5555-CORRENTISTA    PIC  X(001)         VALUE SPACES.

       01  WRK-5555-AREA-SAIDA.
           05  WRK-5555-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5555-COD-SQL-ERRO   PIC S9(003)         VALUE ZEROS.
           05  WRK-5555-DESC-MSG       PIC  X(070)         VALUE SPACES.

       01  IO-PCB.
           05  IO-LTERM                PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(02)          VALUE SPACES.
           05  IO-STATUS               PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(12)          VALUE SPACES.
           05  IO-MODNAME              PIC  X(08)          VALUE SPACES.

       01  ALT-PCB.
           05  ALT-LTERM               PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(02)          VALUE SPACES.
           05  ALT-STATUS              PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(12)          VALUE SPACES.
           05  ALT-MODNAME             PIC  X(08)          VALUE SPACES.

       COPY I#GFCT0M.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DE RESTART        *'.
      *----------------------------------------------------------------*

       COPY I#CKRS01.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*       AREA TO POOL7100*'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA DE TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E8
           END-EXEC.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    CONTROLE GERAL DE PROCESSOS                                 *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*
CAPG=I     CALL 'CKRS1000'.

           PERFORM 0100-INICIALIZAR.

           PERFORM 0600-VERIFICAR-VAZIO.

           PERFORM 0900-PROCESSAR-DADOS
                                       UNTIL WRK-FIM = 'S'.

           PERFORM 0910-PROCESSAR-ARQTB0E7
                                       UNTIL WRK-FIM-TB7 = 'S'.

           PERFORM 0920-PROCESSAR-ARQTB0E8
                                       UNTIL WRK-FIM-TB8 = 'S'.

           PERFORM 1100-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABRIR ARQUIVOS                                              *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 0200-START-RESTART.

           OPEN INPUT  AGENDADO
                       ARQTB0E7
                       ARQTB0E8.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR CONEXAO COM DB2                          *
      *----------------------------------------------------------------*
       0200-START-RESTART              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE SPACES                 TO CK01-PLAN.
           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'I'                    TO CK01-FUNCAO.
0907***    MOVE SPACES                 TO CK01-AREA-RESTART.
0907***    MOVE ZEROS                  TO CK01-TAM-AREA-RESTART.
0907       MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
0907       MOVE LENGTH OF WRK-AREA-RESTART
0907                                   TO CK01-TAM-AREA-RESTART.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

           IF  CK01-RESTART
               MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                       TO WRK-AREA-RESTART
               MOVE WRK-LIDOS-AGENDADO TO ACU-LIDOS-AGENDADO
               MOVE WRK-ATU-GFCTB092   TO ACU-ATU-GFCTB092
               MOVE WRK-LIDOS-ARQTB0E7 TO ACU-LIDOS-ARQTB0E7
               MOVE WRK-ATU-GFCTB0E7   TO ACU-ATU-GFCTB0E7
               MOVE WRK-LIDOS-ARQTB0E8 TO ACU-LIDOS-ARQTB0E8
               MOVE WRK-ATU-GFCTB0E8   TO ACU-ATU-GFCTB0E8
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR ERRO NO ACESSO AO MODULO CKRS0100        *
      *----------------------------------------------------------------*
       0300-TRATAR-ERRO-RESTART        SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           IF  CK01-CODIGO-RETORNO     EQUAL 8
               EVALUATE CK01-CODIGO-MENSAGEM
                   WHEN 'CKRS0001'
                        MOVE 'CKRS0100 - CODIGO DE FUNCAO INCORRETO'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0002'
                        MOVE 'CKRS0100 - JA FOI FEITA UMA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0003'
                        MOVE 'CKRS0100 - NAO FOI EFETUADA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0004'
                        MOVE 'CKRS0100 - CAMPO DB2-ID DEVE ESTAR INFORMA
      -                      'DO'      TO ERR-TEXTO

                   WHEN 'CKRS0005'
                        MOVE 'CKRS0100 - O PROCESSO NAO SE ENCONTRA CADA
      -                      'STRADO'  TO ERR-TEXTO

                   WHEN OTHER
                        MOVE 'CKRS0100 - ERRO NAO IDENTIFICADO'
                                       TO ERR-TEXTO
                        DISPLAY '************** GFCT1647 **************'
                        DISPLAY '*                                    *'
                        DISPLAY '* CK01-CODIGO-RETORNO : '
                                   CK01-CODIGO-RETORNO    '           *'
                        DISPLAY '* CK01-CODIGO-MENSAGEM: '
                                   CK01-CODIGO-MENSAGEM         '     *'
                        DISPLAY '*                                    *'
                        DISPLAY '************** GFCT1647 **************'
               END-EVALUATE
           ELSE
               MOVE 'CKRS0100 - ERRO GRAVE - RETURN <> DE ZEROS E 8'
                                       TO ERR-TEXTO
               DISPLAY '************** GFCT1647 **************'
               DISPLAY '*                                    *'
               DISPLAY '* CK01-CODIGO-RETORNO : '
                          CK01-CODIGO-RETORNO    '           *'
               DISPLAY '* CK01-CODIGO-MENSAGEM: '
                          CK01-CODIGO-MENSAGEM         '     *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT1647 **************'
           END-IF.

           PERFORM 9999-TRATAR-ERRO.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DOS ARQUIVOS                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0500-TESTAR-FS-AGENDADO.

           PERFORM 0510-TESTAR-FS-ARQTB0E7.

           PERFORM 0520-TESTAR-FS-ARQTB0E8.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO AGENDADO               *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-AGENDADO         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-AGENDADO          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'AGENDADO'          TO WRK-NOME-ARQ
              MOVE WRK-FS-AGENDADO     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO ARQTB0E7               *
      *----------------------------------------------------------------*
       0510-TESTAR-FS-ARQTB0E7         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQTB0E7          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'ARQTB0E7'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ARQTB0E7     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO ARQTB0E8               *
      *----------------------------------------------------------------*
       0520-TESTAR-FS-ARQTB0E8         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQTB0E8          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'ARQTB0E8'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ARQTB0E8     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    ROTINA VERIFICAR ARQUIVOS VAZIOS                            *
      *----------------------------------------------------------------*
       0600-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0700-LER-AGENDADO.

           IF WRK-FS-AGENDADO          EQUAL '10'
               DISPLAY '*************** GFCT1647 **************'
               DISPLAY '*                                     *'
               DISPLAY '*       ARQUIVO AGENDADO VAZIO        *'
               DISPLAY '*                                     *'
               DISPLAY '*       TERMINO NORMAL                *'
               DISPLAY '*                                     *'
               DISPLAY '*************** GFCT1647 **************'
           END-IF.

           PERFORM 0710-LER-ARQTB0E7.

           IF WRK-FS-ARQTB0E7          EQUAL '10'
               DISPLAY '*************** GFCT1647 **************'
               DISPLAY '*                                     *'
               DISPLAY '*       ARQUIVO ARQTB0E7 VAZIO        *'
               DISPLAY '*                                     *'
               DISPLAY '*       TERMINO NORMAL                *'
               DISPLAY '*                                     *'
               DISPLAY '*************** GFCT1647 **************'
           END-IF.

           PERFORM 0720-LER-ARQTB0E8.

           IF WRK-FS-ARQTB0E8          EQUAL '10'
               DISPLAY '*************** GFCT1647 **************'
               DISPLAY '*                                     *'
               DISPLAY '*       ARQUIVO ARQTB0E8 VAZIO        *'
               DISPLAY '*                                     *'
               DISPLAY '*       TERMINO NORMAL                *'
               DISPLAY '*                                     *'
               DISPLAY '*************** GFCT1647 **************'
           END-IF.

           PERFORM 0800-ACESSA-GFCTB0A1.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA LER ARQUIVO AGENDADO                                 *
      *----------------------------------------------------------------*
       0700-LER-AGENDADO               SECTION.
      *----------------------------------------------------------------*

           READ AGENDADO.

           IF  WRK-FS-AGENDADO         EQUAL '10'
               MOVE 'S'                TO WRK-FIM
               GO                      TO 0700-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-AGENDADO.

           ADD 1                       TO ACU-LIDOS-AGENDADO.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA LER ARQUIVO ARQTB0E7                                 *
      *----------------------------------------------------------------*
       0710-LER-ARQTB0E7               SECTION.
      *----------------------------------------------------------------*

           READ ARQTB0E7.

           IF  WRK-FS-ARQTB0E7         EQUAL '10'
               MOVE 'S'                TO WRK-FIM-TB7
               GO                      TO 0710-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0510-TESTAR-FS-ARQTB0E7.

           ADD 1                       TO ACU-LIDOS-ARQTB0E7.

      *----------------------------------------------------------------*
       0710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA LER ARQUIVO ARQTB0E8                                 *
      *----------------------------------------------------------------*
       0720-LER-ARQTB0E8               SECTION.
      *----------------------------------------------------------------*

           READ ARQTB0E8.

           IF  WRK-FS-ARQTB0E8         EQUAL '10'
               MOVE 'S'                TO WRK-FIM-TB8
               GO                      TO 0720-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0520-TESTAR-FS-ARQTB0E8.

           ADD 1                       TO ACU-LIDOS-ARQTB0E8.

      *----------------------------------------------------------------*
       0720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE ACESSO A TABELA GFCTB0A1
      *----------------------------------------------------------------*
       0800-ACESSA-GFCTB0A1            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  DPROCM_ATUAL
               INTO   :GFCTB0A1.DPROCM-ATUAL
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0001'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ATUALIZACAO TABELA GFCTB092                                *
      *----------------------------------------------------------------*
       0900-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYJ-DENVIO-MOVTO-TARIF
                                       TO DENVIO-MOVTO-TARIF OF GFCTB092
           MOVE GFCTYJ-CROTNA-ORIGE-MOVTO
                                       TO CROTNA-ORIGE-MOVTO OF GFCTB092
           MOVE GFCTYJ-CNRO-ARQ-MOVTO  TO CNRO-ARQ-MOVTO OF GFCTB092
           MOVE GFCTYJ-CSEQ-MOVTO      TO CSEQ-MOVTO OF GFCTB092
           MOVE GFCTYJ-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB092.
           MOVE GFCTYJ-DAGNDA-DEB-MOVTO
                                       TO DAGNDA-DEB-MOVTO OF GFCTB092
           MOVE GFCTYJ-CSIT-EVNTO-RECBD
                                       TO CSIT-EVNTO-RECBD OF GFCTB092

           EXEC SQL
             UPDATE DB2PRD.MOVTO_EVNTO_CRRTT
               SET
                   DAGNDA_DEB_MOVTO    = :GFCTB092.DAGNDA-DEB-MOVTO,
                   CSIT_EVNTO_RECBD    = :GFCTB092.CSIT-EVNTO-RECBD
             WHERE
                   DENVIO_MOVTO_TARIF  = :GFCTB092.DENVIO-MOVTO-TARIF
             AND   CROTNA_ORIGE_MOVTO  = :GFCTB092.CROTNA-ORIGE-MOVTO
             AND   CNRO_ARQ_MOVTO      = :GFCTB092.CNRO-ARQ-MOVTO
             AND   CSEQ_MOVTO          = :GFCTB092.CSEQ-MOVTO
             AND   CSERVC_TARIF        = :GFCTB092.CSERVC-TARIF
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO ERR-DBD-TAB
                MOVE 'UPDATE    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0002'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

           ADD 1                       TO ACU-ATU-GFCTB092.

           PERFORM 0930-ACESSAR-GFCT5555.

           PERFORM 1000-CHECK-POINT.

           PERFORM 0700-LER-AGENDADO.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ATUALIZACAO TABELA ARQTB0E7                                *
      *----------------------------------------------------------------*
       0910-PROCESSAR-ARQTB0E7         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYK-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0E7

           MOVE GFCTYK-CAGPTO-CTA      TO CAGPTO-CTA  OF GFCTB0E7

           MOVE GFCTYK-DINIC-SUSP-COBR TO DINIC-SUSP-COBR OF GFCTB0E7

           MOVE GFCTYK-CSEQ-AGPTO-CTA  TO CSEQ-AGPTO-CTA OF GFCTB0E7

           MOVE 'E'                    TO CINDCD-LIBRC-COBR OF GFCTB0E7

           EXEC SQL
             UPDATE DB2PRD.SUSP_TARIF_AGPTO
               SET CINDCD_LIBRC_COBR   = :GFCTB0E7.CINDCD-LIBRC-COBR
             WHERE CSERVC_TARIF        = :GFCTB0E7.CSERVC-TARIF
             AND   CAGPTO_CTA          = :GFCTB0E7.CAGPTO-CTA
             AND   DINIC_SUSP_COBR     = :GFCTB0E7.DINIC-SUSP-COBR
             AND   CSEQ_AGPTO_CTA      = :GFCTB0E7.CSEQ-AGPTO-CTA
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'SUSP_TARIF_AGPTO '
                                       TO ERR-DBD-TAB
                MOVE 'UPDATE    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0003'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

           ADD 1                       TO ACU-ATU-GFCTB0E7

           PERFORM 1000-CHECK-POINT.

           PERFORM 0710-LER-ARQTB0E7.

      *----------------------------------------------------------------*
       0910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ATUALIZACAO TABELA ARQTB0E8                                *
      *----------------------------------------------------------------*
       0920-PROCESSAR-ARQTB0E8         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYI-CJUNC-DEPDC     TO CJUNC-DEPDC OF GFCTB0E8

           MOVE GFCTYI-CCTA-CLI        TO CCTA-CLI OF GFCTB0E8

           MOVE GFCTYI-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0E8

           MOVE GFCTYI-DINIC-SUSP-COBR TO DINIC-SUSP-COBR OF GFCTB0E8

           MOVE 'E'                    TO CINDCD-LIBRC-COBR OF GFCTB0E8

           EXEC SQL
             UPDATE DB2PRD.SUSP_TARIF_CTA
               SET CINDCD_LIBRC_COBR   = :GFCTB0E8.CINDCD-LIBRC-COBR
             WHERE CJUNC_DEPDC         = :GFCTB0E8.CJUNC-DEPDC
             AND   CCTA_CLI            = :GFCTB0E8.CCTA-CLI
             AND   CSERVC_TARIF        = :GFCTB0E8.CSERVC-TARIF
             AND   DINIC_SUSP_COBR     = :GFCTB0E8.DINIC-SUSP-COBR
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'SUSP_TARIF_CTA   '
                                       TO ERR-DBD-TAB
                MOVE 'UPDATE    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0004'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

           ADD 1                       TO ACU-ATU-GFCTB0E8

           PERFORM 1000-CHECK-POINT.

           PERFORM 0720-LER-ARQTB0E8.

      *----------------------------------------------------------------*
       0920-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      * REALIZA ACESSO AO MODULO GFCT5555
      ******************************************************************
      *-----------------------------------------------------------
       0930-ACESSAR-GFCT5555           SECTION.
      *-----------------------------------------------------------

           MOVE 'H'                        TO WRK-5555-CORRENTISTA.
           MOVE GFCTYJ-CSIT-EVNTO-RECBD    TO WRK-5555-CSIT-EVNTO
           MOVE GFCTYJ-DENVIO-MOVTO-TARIF  TO WRK-5555-DENVIO-MOVTO.
           MOVE GFCTYJ-CROTNA-ORIGE-MOVTO  TO WRK-5555-CROTNA-ORIGE.
           MOVE GFCTYJ-CNRO-ARQ-MOVTO      TO WRK-5555-CNRO-ARQ.
           MOVE GFCTYJ-CSEQ-MOVTO          TO WRK-5555-CSEQ-MOVTO.
           MOVE 'GFCT1647'                 TO WRK-5555-CHAMADOR.
           MOVE 'GFCT5555'                 TO WRK-MODULO.

           CALL WRK-MODULO             USING  WRK-5555-AREA-ENTRADA
                                              WRK-5555-AREA-SAIDA
                                              GFCT0M-AREA-ERROS
                                              IO-PCB
                                              ALT-PCB.

           IF  WRK-5555-COD-RETORNO        NOT EQUAL ZEROS
               IF  WRK-5555-COD-RETORNO    EQUAL 99
                   MOVE WRK-5555-COD-SQL-ERRO
                                           TO WRK-MASCARA
                   DISPLAY '******************************** GFCT1647 **
      -                    '******************************'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '* ERRO NO MODULO GFCT5555
      -                    '                             *'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '* SQLCODE: ' WRK-MASCARA '
      -                    '                                 *'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '* ' WRK-5555-DESC-MSG ' *'
                   DISPLAY '*
      -                   '                             *'
                   DISPLAY '******************************** GFCT1647 **
      -                    '******************************'
               ELSE
                   MOVE 'APL'              TO ERR-TIPO-ACESSO
                   MOVE WRK-5555-DESC-MSG  TO ERR-TEXTO
               END-IF
               MOVE 'GFCT5555'             TO ERR-MODULO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA DO CHECK POINT                                      *
      *----------------------------------------------------------------*
       1000-CHECK-POINT                SECTION.
      *----------------------------------------------------------------*

0709****   INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'P'                    TO CK01-FUNCAO.
           MOVE ACU-LIDOS-AGENDADO     TO WRK-LIDOS-AGENDADO.
           MOVE ACU-ATU-GFCTB092       TO WRK-ATU-GFCTB092.
           MOVE ACU-LIDOS-ARQTB0E7     TO WRK-LIDOS-ARQTB0E7.
           MOVE ACU-ATU-GFCTB0E7       TO WRK-ATU-GFCTB0E7.
           MOVE ACU-LIDOS-ARQTB0E8     TO WRK-LIDOS-ARQTB0E8.
           MOVE ACU-ATU-GFCTB0E8       TO WRK-ATU-GFCTB0E8.
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS FINAIS                                       *
      *----------------------------------------------------------------*
       1100-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1200-EMITIR-DISPLAY.

           CLOSE AGENDADO
                 ARQTB0E7
                 ARQTB0E8.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FILE-STATUS.

           PERFORM 1300-FINALIZA-CHECK-POINT.

           STOP RUN.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ESTATISTICAS DE PROCESSAMENTO                               *
      *----------------------------------------------------------------*
       1200-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-AGENDADO     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1647 '
                   '********************'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*        PROGRAMA ENCERRADO CO'
                   'M SUCESSO          *'.
           DISPLAY '* ============================'
                   '===============    *'.
           DISPLAY '*   PROCESSAMENTO DE : ' DPROCM-ATUAL OF GFCTB0A1
                   '                *'.
           DISPLAY '* ============================'
                   '=====              *'.
           DISPLAY '*   TOTAL DE LIDOS EM AGENDADO......: '
                   WRK-MASCARA '  *'.

           MOVE ACU-ATU-GFCTB092       TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE ATUALIZADOS GFCTB092...: '
                   WRK-MASCARA '  *'.

           MOVE ACU-LIDOS-ARQTB0E7     TO WRK-MASCARA.
           DISPLAY '*   TOTAL DE LIDOS EM ARQTB0E7......: '
                   WRK-MASCARA '  *'.

           MOVE ACU-ATU-GFCTB0E7       TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE ATUALIZADOS GFCTB0E7...: '
                   WRK-MASCARA '  *'.

           MOVE ACU-LIDOS-ARQTB0E8     TO WRK-MASCARA.
           DISPLAY '*   TOTAL DE LIDOS EM ARQTB0E8......: '
                   WRK-MASCARA '  *'.

           MOVE ACU-ATU-GFCTB0E8       TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE ATUALIZADOS GFCTB0E8...: '
                   WRK-MASCARA '  *'.

           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*                   TERMINO NO'
                   'RMAL               *'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '******************** GFCT1647 '
                   '********************'.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZAR CHECK POINT                                      *
      *----------------------------------------------------------------*
       1300-FINALIZA-CHECK-POINT       SECTION.
      *----------------------------------------------------------------*

0907***    INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'F'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA COMMIT                                              *
      *----------------------------------------------------------------*
      *8888-EXECUTAR-COMMIT            SECTION.
      *----------------------------------------------------------------*
      *
      *    EXEC SQL
      *      COMMIT
      *    END-EXEC.
      *
      *    IF   SQLCODE                NOT EQUAL ZEROS
      *         MOVE 'DB2'             TO ERR-TIPO-ACESSO
      *         MOVE 'COMMIT'          TO ERR-DBD-TAB
      *                                   ERR-FUN-COMANDO
      *         MOVE SQLCODE           TO ERR-SQL-CODE
      *         MOVE '0003'            TO ERR-LOCAL
      *         MOVE SPACES            TO ERR-SEGM
      *         PERFORM 9999-TRATAR-ERRO
      *    END-IF.
      *
      *----------------------------------------------------------------*
      *8888-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA TRATAR ERRO                                         *
      *----------------------------------------------------------------*
       9999-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1647'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
