      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0288.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0288                                    *
      *    PROGRAMADOR.:   ANDRE LUIZ LONGO        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   03/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DE SUPENSAO DE TARIFA.             *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.PARM_SERVC_TARIF       GFCTB0A2       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.SUSP_GRP_CLI           GFCTB0E3       *
      *                   DB2PRD.SUSP_GRP_DEPDC         GFCTB0E4       *
      *                   DB2PRD.SUSP_GRP_MUN           GFCTB0E5       *
      *                   DB2PRD.SUSP_GRP_UF            GFCTB0E6       *
      *                   DB2PRD.SUSP_TARIF_AGPTO       GFCTB0E7       *
      *                   DB2PRD.TSUSP_GRP_PAB          GFCTB0I1       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTP6 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTP7 - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTIU - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5110 *
      *    I#GFCTIV - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5110 *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5522 *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5523 *
      *    I#MESUM6 - AREA DE COMUNICACAO COM O MODULO MESU9015        *
      *    I#MESUM8 - AREA DE COMUNICACAO COM O MODULO MESU9018        *
      *    I#RURC88 - AREA DE COMUNICACAO COM MODULO RURC9020          *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5110 - PESQUISAR BASES CLIE.                            *
      *    RURC9020 - OBTER DESCRICAO MUNICIPIO.                       *
      *    DCITV002 - OBTER DESCRICAO UF.                              *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    MESU9018 - AREA DE COMUNICACAO - ENTRADA DO MODULO          *
      *    MESU9015 - AREA DE COMUNICACAO - SAIDA DO MODULO            *
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
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
       77  WRK-ACHOU-DUPLICIDADE       PIC  X(001)         VALUE SPACES.
       77  WRK-SEQUENCIA               PIC  9(009)         VALUE ZEROS.

       01  WRK-COUNT                   PIC S9(009) COMP    VALUE ZEROS.

       01  WRK-FUNC-BDSCO-X.
           05  WRK-FUNC-BDSCO          PIC  X(007)         VALUE SPACES.
       01  WRK-FUNC-BDSCO-R            REDEFINES WRK-FUNC-BDSCO-X
                                       PIC  9(007).

       01  WRK-CONTROLE-X.
           05  WRK-CONTROLE            PIC  X(002)         VALUE SPACES.
       01  WRK-CONTROLE-R              REDEFINES WRK-CONTROLE-X
                                       PIC  9(002).

       01  WRK-DATA-FIM.
           05  WRK-ANO-FIM             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-FIM             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-FIM             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-INICIO.
           05  WRK-ANO-INI             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-INI             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-INI             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-PROCM.
           05  WRK-ANO-PROCM           PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-PROCM           PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-PROCM           PIC  9(002)         VALUE ZEROS.

       01  WRK-CAD-DATA.
           05  WRK-CAD-DIA             PIC  9(002)         VALUE ZEROS.
           05  WRK-CAD-MES             PIC  9(002)         VALUE ZEROS.
           05  WRK-CAD-ANO             PIC  9(004)         VALUE ZEROS.

       01  WRK-MSG01.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MOD             PIC  X(008)         VALUE SPACES.

ST2506*----------------------------------------------------------------*
ST2506*01  FILLER                      PIC  X(050)         VALUE
ST2506*    '*** AREAS DA POOL0110 ***'.
ST2506*----------------------------------------------------------------*
ST2506*
ST2506*01  WRK-CGC-CPF-0110.
ST2506*    05  WRK-NUMERO-0110         PIC  9(009)         VALUE ZEROS.
ST2506*    05  WRK-FILIAL-0110         PIC  9(005)         VALUE ZEROS.
ST2506*
ST2506*01  WRK-CONTROLE-0110           PIC  X(002)         VALUE SPACES.

ST2506*----------------------------------------------------------------*
ST2506 01  FILLER                      PIC  X(050)         VALUE
ST2506    '*** AREAS DA BRAD2000 ***'.
ST2506*----------------------------------------------------------------*
ST2506
ST2506 01  WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506     'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.        
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.
ST2506 01  WRK-9-X-2.
ST2506     05 WRK-9-02                 PIC 9(02)           VALUE ZEROS.
ST2506*------------------------------------*----------------------------*
ST2506 01 FILLER                       PIC  X(051) VALUE
ST2506       '* AREA PARA MODULO GFCT20PJ *'.
ST2506*---------------------------------------------------------------*
ST2506 01 WRK-GFCT20PJ                 PIC X(08)   VALUE
ST2506       'GFCT20PJ'.
ST2506
ST2506 01 WRK-AREA-GFCT20PJ.
ST2506 COPY 'GFCTW2PJ'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MODULO DCIT8000 *'.
      *---------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

       01  WRK-ERRO-AREA                PIC  X(107)        VALUE SPACES.
       01  WRK-SQLCA-DCIT               PIC  X(136)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA INCLUDES *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#GFCTIU'.

       COPY 'I#GFCTIV'.

       COPY 'I#RURC88'.

       COPY 'I#MESUM6'.

       COPY 'I#MESUM8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E4
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E5
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE  CSR01-GFCTB0E7   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA
             FROM  DB2PRD.SUSP_TARIF_AGPTO
             WHERE
                   CSERVC_TARIF       = :GFCTB0E7.CSERVC-TARIF      AND
                   DFIM_SUSP_COBR     > :GFCTB0E7.DFIM-SUSP-COBR    AND
                   CAGPTO_CTA         = :GFCTB0E7.CAGPTO-CTA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTP6'.

       COPY 'I#GFCTP7'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTP6-ENTRADA
                                                      GFCTP7-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA INICIAL DO PROGRAMA                                      *
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
      *MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E CON- *
      *SISTE DADOS.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTP7-SAIDA.

           INITIALIZE GFCTP7-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                         TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE 'GFCT5522'                     TO WRK-MODULO.
           MOVE 'GFCT'                         TO GFCTFZ-SISTEMA.
           MOVE GFCTP6-TRANSACAO               TO GFCTFZ-TRANSACAO.
           MOVE GFCTP6-FUNCAO                  TO GFCTFZ-FUNCAO.
           MOVE GFCTP6-FUNC-BDSCO              TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTFZ-FIM.

           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTFZ-ENTRADA
                                                     GFCTG1-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTP7-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0288'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                     EQUAL 9
               MOVE 'GFCT0288'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO        TO GFCTP7-COD-SQL-ERRO
                                                  GFCT0M-SQLCODE
               MOVE GFCTG1-ERRO                TO GFCTP7-ERRO
               MOVE GFCTG1-COD-MSG-ERRO        TO GFCTP7-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO       TO GFCT0M-TEXTO
                                                  GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE              EQUAL 1
               MOVE 9                          TO GFCTP7-ERRO
               MOVE 'SISTEMA INDISPONIVEL'     TO GFCT0M-TEXTO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0288'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE +1010                      TO GFCTP7-LL.
           MOVE ZEROS                      TO GFCTP7-ZZ.
           MOVE GFCTP6-TRANSACAO           TO GFCTP7-TRANSACAO.
           MOVE GFCTP6-FUNCAO              TO GFCTP7-FUNCAO.
           MOVE GFCTP6-FUNC-BDSCO          TO GFCTP7-FUNC-BDSCO.
           MOVE 'S'                        TO GFCTP7-FIM.
           MOVE GFCTP6-FIXO                TO GFCTP7-FIXO.

           MOVE ZEROS                      TO GFCTP7-ERRO
                                              GFCTP7-COD-SQL-ERRO
                                              GFCTP7-COD-MSG-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG           TO GFCTP7-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTP6-TRANSACAO        EQUAL LOW-VALUES        OR
               GFCTP6-TRANSACAO        EQUAL SPACES            OR
               GFCTP6-FUNCAO           EQUAL LOW-VALUES        OR
               GFCTP6-FUNCAO           EQUAL SPACES            OR
               GFCTP6-FUNC-BDSCO       EQUAL LOW-VALUES        OR
               GFCTP6-FUNC-BDSCO       EQUAL SPACES            OR
               GFCTP6-DATA-INICIO      EQUAL LOW-VALUES        OR
               GFCTP6-DATA-INICIO      EQUAL SPACES            OR
               GFCTP6-DATA-FIM         EQUAL LOW-VALUES        OR
               GFCTP6-DATA-FIM         EQUAL SPACES            OR
               GFCTP6-CTARIFA          NOT NUMERIC             OR
               GFCTP6-CTARIFA          EQUAL ZEROS             OR
              (GFCTP6-TOTAL            NOT EQUAL 'S' AND 'N')  OR
              (GFCTP6-CAGPTO           NOT EQUAL 4 AND 14 AND 15 AND 18
                                       AND 19)
               MOVE 1                  TO GFCTP7-ERRO
                                          GFCTP7-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE GFCTP6-CAGPTO

               WHEN 4
      
ST2506            MOVE GFCTP6-CGC-CPF         TO GFCTW2PJ-E-CGC-CNPJ 
ST2506            PERFORM 1301-VALIDAR-FORMATO-PJ

ST2506            IF  GFCTP6-CGC-CPF       EQUAL SPACES OR
ST2506                GFCTP6-CGC-CPF       EQUAL LOW-VALUES OR
ST2506                GFCTW2PJ-S-INVALIDO 

ST2506*           IF  GFCTP6-CGC-CPF       NOT NUMERIC OR
ST2506*               GFCTP6-CGC-CPF       EQUAL ZEROS
                      MOVE 1               TO GFCTP7-ERRO
                      MOVE 773             TO GFCTP7-COD-MSG-ERRO
                      MOVE '0773'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTP7-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTP7-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 14
                  IF (GFCTP6-CAGENCIA      NOT NUMERIC       ) OR
                     (( GFCTP6-CAGENCIA    EQUAL ZEROS   ) AND
                      ( GFCTP7-TOTAL       NOT EQUAL 'S' ) )
                      MOVE 1               TO GFCTP7-ERRO
                      MOVE 774             TO GFCTP7-COD-MSG-ERRO
                      MOVE '0774'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTP7-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTP7-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 15
                  IF (GFCTP6-CPAB          NOT NUMERIC) OR
                     (GFCTP6-CAGENCIA      NOT NUMERIC OR
                      GFCTP6-CAGENCIA      EQUAL ZEROS) OR
                     (GFCTP6-CPAB          EQUAL ZEROS AND
                      GFCTP7-TOTAL         NOT EQUAL 'S')
                      MOVE 1               TO GFCTP7-ERRO
                      MOVE 776             TO GFCTP7-COD-MSG-ERRO
                      MOVE '0776'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTP7-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTP7-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 18
                  IF  GFCTP6-CMUNIC        EQUAL SPACES OR
                      GFCTP6-CMUNIC        EQUAL LOW-VALUES
                      MOVE 1               TO GFCTP7-ERRO
                      MOVE 779             TO GFCTP7-COD-MSG-ERRO
                      MOVE '0779'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTP7-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTP7-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 19
                  IF  GFCTP6-CUF           EQUAL SPACES OR
                      GFCTP6-CUF           EQUAL LOW-VALUES
                      MOVE 1               TO GFCTP7-ERRO
                      MOVE 780             TO GFCTP7-COD-MSG-ERRO
                      MOVE '0780'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTP7-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTP7-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF
           END-EVALUATE.

           IF  GFCTP6-JUST                 EQUAL SPACES
               MOVE 1                      TO GFCTP7-ERRO
               MOVE 884                    TO GFCTP7-COD-MSG-ERRO
               MOVE '0884'                 TO GFCTG2-COD-MSG
               MOVE ZEROS                  TO GFCTP7-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1320-ACESSAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1301-VALIDAR-FORMATO-PJ         SECTION.
      *----------------------------------------------------------------*
        
           CALL WRK-GFCT20PJ           USING WRK-AREA-GFCT20PJ.

           IF RETURN-CODE NOT EQUAL ZEROS 
           OR GFCTW2PJ-S-COD-RETORNO NOT EQUAL '00'
              MOVE 9                          TO GFCTP7-ERRO
              MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MOD
              MOVE WRK-MSG01                  TO GFCT0M-TEXTO
              MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
              MOVE 'GFCT0288'                 TO GFCT0M-TRANSACAO
              PERFORM 3000-FINALIZAR
           END-IF. 
                                       
      *----------------------------------------------------------------*
       1301-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTP6-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTP6-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTP6-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTP7-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MOD
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0288'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 9
               MOVE 9                          TO GFCTP7-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MOD
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0288'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 1
               MOVE 1                          TO GFCTP7-ERRO
               MOVE GFCTG3-COD-SQL-ERRO        TO GFCTP7-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO        TO GFCTP7-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO       TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCTB0A1 PARA BUSCAR DATA                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-ACESSAR-GFCTB0A1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO  :GFCTB0A1.DPROCM-ATUAL
             FROM  DB2PRD.PARM_DATA_PROCM
             WHERE CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOM-TAB
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MOVER ERRO SQL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1321-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTP7-ERRO.
           MOVE 'GFCT0288'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTP7-COD-SQL-ERRO.
           MOVE SPACES                 TO GFCTP7-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG      WRK-NOM-TAB

           DELIMITED BY '  '           INTO GFCTP7-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTP6-CAGPTO
               WHEN 15
                   PERFORM 2100-CONSISTIR-PAB
               WHEN 14
                   IF  GFCTP6-TOTAL    NOT EQUAL 'S'
                       PERFORM 2200-CONSISTIR-AGENCIA
                   END-IF
               WHEN 4
                   PERFORM 2300-CONSISTIR-CLIENTE
               WHEN 18
                   PERFORM 2400-CONSISTIR-MUNICIPIO
               WHEN 19
                   PERFORM 2500-CONSISTIR-UF
           END-EVALUATE.

           PERFORM 2600-CONSISTIR-DATA.

           PERFORM 2700-VERIFICAR-DUPLICIDADE.

           PERFORM 2800-INSERIR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CURSOR1                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSISTIR-PAB              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-CONSISTIR-AGENCIA.

           IF  GFCTP6-TOTAL            NOT EQUAL 'S'
               MOVE 'VRS001'           TO COMU-VERSAO
               MOVE 237                TO COMU-COD-EMPR
               MOVE GFCTP6-CAGENCIA    TO COMU-COD-DEPDC
               MOVE GFCTP6-CPAB        TO COMU-COD-POSTO
               MOVE SPACES             TO COMU-CORR-BANCARIO
               MOVE 'MESU9018'         TO WRK-MODULO
               CALL 'POOL0081'         USING WRK-MODULO
                                             COMU-DADOS-9018
              IF  RETURN-CODE          NOT EQUAL ZEROS
                  IF  RETURN-CODE      EQUAL 4
                      MOVE 1           TO GFCTP7-ERRO
                      MOVE 0518        TO GFCTP7-COD-MSG-ERRO
                      MOVE '0518'      TO GFCTG2-COD-MSG
                      MOVE ZEROS       TO GFCTP7-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG
                                       TO GFCTP7-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  ELSE
                      IF  RETURN-CODE  EQUAL 8
                           MOVE 1      TO GFCTP7-ERRO
                           MOVE 0519   TO GFCTP7-COD-MSG-ERRO
                           MOVE '0519' TO GFCTG2-COD-MSG
                           MOVE ZEROS  TO GFCTP7-COD-SQL-ERRO
                           PERFORM 1310-OBTER-DESC-MSG
                           MOVE GFCTG3-DESC-MSG
                                       TO GFCTP7-DESC-MSG-ERRO
                           PERFORM 3000-FINALIZAR
                      ELSE
                           IF  RETURN-CODE   EQUAL 12
                               MOVE 1        TO GFCTP7-ERRO
                               MOVE 0520     TO GFCTP7-COD-MSG-ERRO
                               MOVE '0520'   TO GFCTG2-COD-MSG
                               MOVE ZEROS    TO GFCTP7-COD-SQL-ERRO
                               PERFORM 1310-OBTER-DESC-MSG
                               MOVE GFCTG3-DESC-MSG
                                             TO GFCTP7-DESC-MSG-ERRO
                               PERFORM 3000-FINALIZAR
                           END-IF
                      END-IF
                  END-IF
              ELSE
                 IF  COMU-COD-TIPO-POSTO   EQUAL 9
                     MOVE 1                TO GFCTP7-ERRO
                     MOVE 0594             TO GFCTP7-COD-MSG-ERRO
                     MOVE '0594'           TO GFCTG2-COD-MSG
                     MOVE ZEROS            TO GFCTP7-COD-SQL-ERRO
                     PERFORM 1310-OBTER-DESC-MSG
                     MOVE GFCTG3-DESC-MSG  TO GFCTP7-DESC-MSG-ERRO
                     PERFORM 3000-FINALIZAR
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR AGENCIA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSISTIR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO COMU-BANCO.
           MOVE GFCTP7-CAGENCIA        TO COMU-DEPDC.
           MOVE 'GFCT0288'             TO COMU-NOME-PGM.
           MOVE 'MESU9015'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  COMU-COD-RETORNO         NOT EQUAL ZEROS
               MOVE 1                   TO GFCTP7-ERRO
               MOVE 0465                TO GFCTP7-COD-MSG-ERRO
               MOVE '0465'              TO GFCTG2-COD-MSG
               MOVE ZEROS               TO GFCTP7-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR CLIENTE                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSISTIR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

ST2506*    MOVE SPACES                 TO WRK-CONTROLE-0110.
ST2506*    MOVE GFCTP6-CGC-CPF         TO WRK-NUMERO-0110.
ST2506*    MOVE GFCTP6-FILIAL          TO WRK-FILIAL-0110.
ST2506*
ST2506*    CALL 'POOL0110'             USING WRK-CGC-CPF-0110
ST2506*                                      WRK-CONTROLE-0110.

ST2506     MOVE ZEROS          TO WRK-CAD-CONTROLE. 
ST2506     MOVE GFCTP6-CGC-CPF TO WRK-CAD-NUMERO.
ST2506     MOVE GFCTP6-FILIAL  TO WRK-CAD-FILIAL.
ST2506
ST2506     CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
ST2506                                 WRK-CAD-CONTROLE.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTP7-ERRO
               MOVE 0468               TO GFCTP7-COD-MSG-ERRO
               MOVE '0468'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

ST2506*    IF  GFCTP6-CONTROLE         NOT EQUAL WRK-CONTROLE-0110
ST2506     MOVE WRK-CAD-CONTROLE       TO WRK-9-02.
ST2506     IF  GFCTP6-CONTROLE         NOT EQUAL WRK-9-X-2 
               MOVE 1                  TO GFCTP7-ERRO
               MOVE 0507               TO GFCTP7-COD-MSG-ERRO
               MOVE '0507'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2310-PESQUISAR-BASE-CLIE.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PESQUISAR BASE CLIENTE                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-PESQUISAR-BASE-CLIE        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIU-ENTRADA
                                          GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTIU-ENTRADA
                      GFCTIV-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTIU-LL.
           MOVE ZEROS                  TO GFCTIU-ZZ.
           MOVE GFCTP6-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTP6-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE GFCTP6-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST2506*    M0VE GFCTP6-CGC-CPF         TO GFCTIU-NRO-CPF-CNPJ.
ST2506*    M0VE GFCTP6-FILIAL          TO GFCTIU-NRO-FILIAL.
ST2506     MOVE GFCTP6-CGC-CPF         TO GFCTIU-COD-CPF-CNPJ.
ST2506     MOVE GFCTP6-FILIAL          TO GFCTIU-COD-FILIAL.
           MOVE GFCTP6-CONTROLE        TO WRK-CONTROLE.
           MOVE WRK-CONTROLE-R         TO GFCTIU-CTRL-CPF-CNPJ.
           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL 'POOL0081'              USING WRK-MODULO
                                              GFCTIU-ENTRADA
                                              GFCTIV-SAIDA
                                              GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTP7-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0288'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTIV-ERRO             NOT EQUAL ZEROS
               MOVE GFCTIV-ERRO        TO GFCTP7-ERRO
               MOVE GFCTIV-COD-SQL-ERRO TO GFCTP7-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO TO GFCTP7-COD-MSG-ERRO
               STRING 'GFCT5110 - '     GFCTIV-DESC-MSG-ERRO
               DELIMITED BY SIZE       INTO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR MUNICIPIO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-CONSISTIR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE GFCTP6-CMUNIC          TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0732               TO GFCTP7-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0675               TO GFCTP7-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0733               TO GFCTP7-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTP7-ERRO
               MOVE SQLCODE-RURC88     TO GFCTP7-COD-SQL-ERRO
               MOVE 0734               TO GFCTP7-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR UF                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-CONSISTIR-UF               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP6-CUF             TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA-DCIT.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0735               TO GFCTP7-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0736               TO GFCTP7-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0737               TO GFCTP7-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0738               TO GFCTP7-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL  4
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'DCITV002'         TO WRK-NOM-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0047'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTP7-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTP7-ERRO
               MOVE 'GFCT0288'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA-DCIT     TO SQLCA
                                          GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTP7-COD-SQL-ERRO
               MOVE SPACES             TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTIR DATAS                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           STRING GFCTP6-DATA-INICIO(07:04)
                  GFCTP6-DATA-INICIO(04:02)
                  GFCTP6-DATA-INICIO(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-INICIO.

           STRING GFCTP6-DATA-FIM(07:04)
                  GFCTP6-DATA-FIM(04:02)
                  GFCTP6-DATA-FIM(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-FIM.

           STRING DPROCM-ATUAL         OF GFCTB0A1(07:04)
                  DPROCM-ATUAL         OF GFCTB0A1(04:02)
                  DPROCM-ATUAL         OF GFCTB0A1(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-PROCM.

           MOVE WRK-DIA-INI            TO WRK-CAD-DIA.
           MOVE WRK-MES-INI            TO WRK-CAD-MES.
           MOVE WRK-ANO-INI            TO WRK-CAD-ANO.

           CALL 'POOL1470'             USING WRK-CAD-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0885               TO GFCTP7-COD-MSG-ERRO
               MOVE '0885'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-DIA-FIM            TO WRK-CAD-DIA.
           MOVE WRK-MES-FIM            TO WRK-CAD-MES.
           MOVE WRK-ANO-FIM            TO WRK-CAD-ANO.

           CALL 'POOL1470'             USING WRK-CAD-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0886               TO GFCTP7-COD-MSG-ERRO
               MOVE '0886'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DATA-INICIO         NOT GREATER WRK-DATA-PROCM
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0887               TO GFCTP7-COD-MSG-ERRO
               MOVE '0887'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DATA-FIM            LESS  WRK-DATA-INICIO
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0888               TO GFCTP7-COD-MSG-ERRO
               MOVE '0888'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2610-VER-VIGENCIA-TARIF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR VIGENCIA NA TARIFA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-VER-VIGENCIA-TARIF         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP6-CTARIFA         TO CSERVC-TARIF      OF GFCTB0A2.

           INSPECT GFCTP6-DATA-INICIO  REPLACING ALL '/'    BY '.'.
           INSPECT GFCTP6-DATA-FIM     REPLACING ALL '/'    BY '.'.

           MOVE GFCTP6-DATA-INICIO     TO DINIC-VGCIA-TARIF OF GFCTB0A2
           MOVE GFCTP6-DATA-FIM        TO DFIM-VGCIA-TARIF  OF GFCTB0A2

           EXEC SQL
             SELECT  B.CPSSOA_SERVC_TARIF
             INTO    :GFCTB0D8.CPSSOA-SERVC-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF A,
                    DB2PRD.SERVC_TARIF_PRINC B
             WHERE  A.CSERVC_TARIF       =  B.CSERVC_TARIF
               AND  A.CSERVC_TARIF       =  :GFCTB0A2.CSERVC-TARIF
               AND  A.DINIC_VGCIA_TARIF <=  :GFCTB0A2.DINIC-VGCIA-TARIF
               AND  A.DFIM_VGCIA_TARIF  >=  :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A2'      TO WRK-NOM-TAB
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0889               TO GFCTP7-COD-MSG-ERRO
               MOVE '0889'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTP6-CAGPTO               EQUAL 4
               IF  GFCTP6-FILIAL           EQUAL ZEROS
                   IF  CPSSOA-SERVC-TARIF  OF GFCTB0D8
                                           NOT EQUAL '1' AND '3'
                       MOVE 1              TO GFCTP7-ERRO
                       MOVE ZEROS          TO GFCTP7-COD-SQL-ERRO
                       MOVE 0891           TO GFCTP7-COD-MSG-ERRO
                       MOVE '0891'         TO GFCTG2-COD-MSG
                       PERFORM 1310-OBTER-DESC-MSG
                       MOVE GFCTG3-DESC-MSG TO GFCTP7-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   END-IF
               ELSE
                   IF  CPSSOA-SERVC-TARIF  OF GFCTB0D8
                                           NOT EQUAL '2' AND '3'
                       MOVE 1              TO GFCTP7-ERRO
                       MOVE ZEROS          TO GFCTP7-COD-SQL-ERRO
                       MOVE 0892           TO GFCTP7-COD-MSG-ERRO
                       MOVE '0892'         TO GFCTG2-COD-MSG
                       PERFORM 1310-OBTER-DESC-MSG
                       MOVE GFCTG3-DESC-MSG TO GFCTP7-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR DUPLICIDADE                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-VERIFICAR-DUPLICIDADE      SECTION.
      *----------------------------------------------------------------*

           PERFORM 2710-ABRIR-CURSOR.

           PERFORM 2720-LER-CURSOR.

           PERFORM 2730-PROCESSAR-CURSOR UNTIL
              WRK-FIM                  EQUAL 'S' OR
              WRK-ACHOU-DUPLICIDADE    EQUAL 'S'.

           PERFORM 2740-FECHAR-CURSOR.

           IF  WRK-ACHOU-DUPLICIDADE   EQUAL 'S'
               MOVE 1                  TO GFCTP7-ERRO
               MOVE ZEROS              TO GFCTP7-COD-SQL-ERRO
               MOVE 0890               TO GFCTP7-COD-MSG-ERRO
               MOVE '0890'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTP7-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM.
           MOVE 'N'                    TO WRK-ACHOU-DUPLICIDADE.
           MOVE GFCTP6-CTARIFA         TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE GFCTP6-CAGPTO          TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE GFCTP6-DATA-INICIO     TO DFIM-SUSP-COBR   OF GFCTB0E7.

           EXEC SQL
              OPEN CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E7'      TO WRK-NOM-TAB
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR                                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0E7   INTO
                   :GFCTB0E7.CSERVC-TARIF,
                   :GFCTB0E7.DINIC-SUSP-COBR,
                   :GFCTB0E7.CAGPTO-CTA,
                   :GFCTB0E7.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E7'      TO WRK-NOM-TAB
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 'S'                 TO WRK-FIM
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2730-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTP6-CAGPTO
               WHEN 14
                   PERFORM 2731-VERIFICAR-AGENCIA
               WHEN 4
                   PERFORM 2732-VERIFICAR-CLIENTE
               WHEN 15
                   PERFORM 2733-VERIFICAR-POSTO
               WHEN 18
                   PERFORM 2734-VERIFICAR-MUNICIPIO
               WHEN 19
                   PERFORM 2735-VERIFICAR-UF
           END-EVALUATE.

           PERFORM 2720-LER-CURSOR.

      *----------------------------------------------------------------*
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR AGENCIA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2731-VERIFICAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA        OF GFCTB0E4.
           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF      OF GFCTB0E4.
*********  MOVE DPROCM-ATUAL           OF GFCTB0A1
*********                              TO DINIC-SUSP-COBR   OF GFCTB0E4.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0E4.

           IF  GFCTP6-TOTAL            EQUAL 'S'
               MOVE ZEROS              TO CDEPDC            OF GFCTB0E4
           ELSE
               MOVE GFCTP6-CAGENCIA    TO CDEPDC            OF GFCTB0E4
           END-IF.

           EXEC SQL
             SELECT COUNT(*)
             INTO  :WRK-COUNT
             FROM   DB2PRD.SUSP_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB0E4.CSERVC-TARIF       AND
*************      DINIC_SUSP_COBR   <= :GFCTB0E4.DINIC-SUSP-COBR    AND
                   CAGPTO_CTA         = :GFCTB0E4.CAGPTO-CTA         AND
                   CEMPR_INC          = :GFCTB0E4.CEMPR-INC          AND
                   CDEPDC             = :GFCTB0E4.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E4'      TO WRK-NOM-TAB
               MOVE 'SUSP_GRP_DEPDC'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       2731-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR CLIENTE                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2732-VERIFICAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA        OF GFCTB0E3.
           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF      OF GFCTB0E3.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-SUSP-COBR   OF GFCTB0E3.
ST2506*    M0VE GFCTP6-CGC-CPF         TO CCGC-CPF          OF GFCTB0E3.
ST2506*    M0VE GFCTP6-FILIAL          TO CFLIAL-CGC        OF GFCTB0E3.
ST2505*    M0VE GFCTP6-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0E3.
ST2506     MOVE GFCTP6-CGC-CPF         TO CCGC-CPF-ST       OF GFCTB0E3.
ST2506     MOVE GFCTP6-FILIAL          TO CFLIAL-CGC-ST     OF GFCTB0E3.
ST2505     MOVE GFCTP6-CONTROLE        TO CCTRL-CPF-CGC-ST  OF GFCTB0E3.

           EXEC SQL
             SELECT COUNT(*)
             INTO  :WRK-COUNT
             FROM   DB2PRD.SUSP_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB0E3.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0E3.CAGPTO-CTA         AND
*************      DINIC_SUSP_COBR   <= :GFCTB0E3.DINIC-SUSP-COBR    AND
ST2506*            CCGC_CPF           = :GFCTB0E3.CCGC-CPF           AND
ST2506*            CFLIAL_CGC         = :GFCTB0E3.CFLIAL-CGC         AND
ST2506*            CCTRL_CPF_CGC      = :GFCTB0E3.CCTRL-CPF-CGC
ST2506             CCGC_CPF_ST        = :GFCTB0E3.CCGC-CPF-ST        AND
ST2506             CFLIAL_CGC_ST      = :GFCTB0E3.CFLIAL-CGC-ST      AND
ST2506             CCTRL_CPF_CGC_ST   = :GFCTB0E3.CCTRL-CPF-CGC-ST 
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E3'      TO WRK-NOM-TAB
               MOVE 'SUSP_GRP_CLI'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       2732-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR POSTO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2733-VERIFICAR-POSTO           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA        OF GFCTB0I1.
           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF      OF GFCTB0I1.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-SUSP-COBR   OF GFCTB0I1.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0I1.
           MOVE GFCTP6-CAGENCIA        TO CDEPDC            OF GFCTB0I1.

           IF  GFCTP6-TOTAL            EQUAL 'S'
               MOVE ZEROS              TO CPOSTO-SERVC      OF GFCTB0I1
           ELSE
               MOVE GFCTP6-CPAB        TO CPOSTO-SERVC      OF GFCTB0I1
           END-IF.

           EXEC SQL
             SELECT COUNT(*)
             INTO  :WRK-COUNT
             FROM    DB2PRD.TSUSP_GRP_PAB
             WHERE   CAGPTO_CTA         = :GFCTB0I1.CAGPTO-CTA
               AND   CSERVC_TARIF       = :GFCTB0I1.CSERVC-TARIF
************   AND   DINIC_SUSP_COBR   <= :GFCTB0I1.DINIC-SUSP-COBR
               AND   CEMPR_INC          = :GFCTB0I1.CEMPR-INC
               AND   CDEPDC             = :GFCTB0I1.CDEPDC
               AND   CPOSTO_SERVC       = :GFCTB0I1.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I1'      TO WRK-NOM-TAB
               MOVE 'TSUSP_GRP_PAB '   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       2733-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR MUNICIPIO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2734-VERIFICAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA        OF GFCTB0E5.
           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF      OF GFCTB0E5.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-SUSP-COBR   OF GFCTB0E5.
           MOVE GFCTP6-CMUNIC          TO CMUN-IBGE         OF GFCTB0E5.

           EXEC SQL
             SELECT COUNT(*)
             INTO  :WRK-COUNT
             FROM   DB2PRD.SUSP_GRP_MUN
               WHERE   CAGPTO_CTA         = :GFCTB0E5.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0E5.CSERVC-TARIF
*************    AND   DINIC_SUSP_COBR   <= :GFCTB0E5.DINIC-SUSP-COBR
                 AND   CMUN_IBGE          = :GFCTB0E5.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E5'      TO WRK-NOM-TAB
               MOVE 'SUSP_GRP_MUN'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       2734-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR UF                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2735-VERIFICAR-UF               SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA        OF GFCTB0E6.
           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF      OF GFCTB0E6.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-SUSP-COBR   OF GFCTB0E6.
           MOVE GFCTP6-CUF             TO CSGL-UF           OF GFCTB0E6.

           EXEC SQL
             SELECT COUNT(*)
             INTO  :WRK-COUNT
             FROM   DB2PRD.SUSP_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB0E6.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB0E6.CAGPTO-CTA         AND
***************    DINIC_SUSP_COBR   <= :GFCTB0E6.DINIC-SUSP-COBR    AND
                   CSGL_UF            = :GFCTB0E6.CSGL-UF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E6'      TO WRK-NOM-TAB
               MOVE 'SUSP_GRP_UF'      TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       2735-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2740-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E7'      TO WRK-NOM-TAB
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2934-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *INSERIR                                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-INSERIR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 2810-INSERIR-E7.

           EVALUATE GFCTP6-CAGPTO
               WHEN 15
                   PERFORM 2820-GERAR-PAB
               WHEN 14
                   PERFORM 2830-GERAR-AGENCIA
               WHEN 4
                   PERFORM 2840-GERAR-CLIENTE
               WHEN 18
                   PERFORM 2850-GERAR-MUNICIPIO
               WHEN 19
                   PERFORM 2860-GERAR-UF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *INSERIR NA TABELA GFCTB0E7                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-INSERIR-E7                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 2811-OBTER-MAIOR-SEQ.

           MOVE GFCTP6-CAGPTO          TO CAGPTO-CTA        OF GFCTB0E7.
           MOVE GFCTP6-CTARIFA         TO CSERVC-TARIF      OF GFCTB0E7.
           MOVE GFCTP6-DATA-INICIO     TO DINIC-SUSP-COBR   OF GFCTB0E7.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0E7.
           MOVE GFCTP6-FUNC-BDSCO      TO WRK-FUNC-BDSCO.
           MOVE WRK-FUNC-BDSCO-R       TO CFUNC-MANUT-INCL  OF GFCTB0E7.
           MOVE ZEROS                  TO CFUNC-MANUT       OF GFCTB0E7.
           MOVE GFCTP6-DATA-FIM        TO DFIM-SUSP-COBR    OF GFCTB0E7.
           MOVE '01.01.0001'           TO DCOBR-SUSP-LIBRD  OF GFCTB0E7.
           MOVE GFCTP6-TOTAL           TO CINDCD-AGPTO-TOT  OF GFCTB0E7.
RIC   *    MOVE SPACES                 TO CINDCD-LIBRC-COBR OF GFCTB0E7
RIC        MOVE 'N'                    TO CINDCD-LIBRC-COBR OF GFCTB0E7
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG        OF GFCTB0E7.
           MOVE LENGTH                 OF GFCTP6-JUST
                                    TO RJUSTF-SUSP-COBR-LEN OF GFCTB0E7.
           MOVE GFCTP6-JUST            TO RJUSTF-SUSP-COBR-TEXT
                                                            OF GFCTB0E7.

           EXEC SQL
             INSERT INTO DB2PRD.SUSP_TARIF_AGPTO
                  (CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   CFUNC_MANUT,
                   CFUNC_MANUT_INCL,
                   DFIM_SUSP_COBR,
                   CINDCD_LIBRC_COBR,
                   DCOBR_SUSP_LIBRD,
                   RJUSTF_SUSP_COBR,
                   CINDCD_AGPTO_TOT,
                   HMANUT_REG,
                   HINCL_REG_SIST)
               VALUES
                  (:GFCTB0E7.CSERVC-TARIF,
                   :GFCTB0E7.DINIC-SUSP-COBR,
                   :GFCTB0E7.CAGPTO-CTA,
                   :GFCTB0E7.CSEQ-AGPTO-CTA,
                   :GFCTB0E7.CFUNC-MANUT,
                   :GFCTB0E7.CFUNC-MANUT-INCL,
                   :GFCTB0E7.DFIM-SUSP-COBR,
                   :GFCTB0E7.CINDCD-LIBRC-COBR,
                   :GFCTB0E7.DCOBR-SUSP-LIBRD,
                   :GFCTB0E7.RJUSTF-SUSP-COBR,
                   :GFCTB0E7.CINDCD-AGPTO-TOT,
                   :GFCTB0E7.HMANUT-REG,
                   CURRENT TIMESTAMP)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E7'      TO WRK-NOM-TAB
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER MAIOR SEQUENCIA                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2811-OBTER-MAIOR-SEQ            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP6-CAGPTO         TO CAGPTO-CTA         OF GFCTB0E7.
           MOVE GFCTP6-CTARIFA        TO CSERVC-TARIF       OF GFCTB0E7.
           MOVE GFCTP6-DATA-INICIO    TO DINIC-SUSP-COBR    OF GFCTB0E7.

           EXEC SQL
               SELECT   MAX(CSEQ_AGPTO_CTA)
               INTO     :GFCTB0E7.CSEQ-AGPTO-CTA
               FROM     DB2PRD.SUSP_TARIF_AGPTO
               WHERE    CAGPTO_CTA        = :GFCTB0E7.CAGPTO-CTA
                 AND    CSERVC_TARIF      = :GFCTB0E7.CSERVC-TARIF
                 AND    DINIC_SUSP_COBR   = :GFCTB0E7.DINIC-SUSP-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E7'      TO WRK-NOM-TAB
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE 1                  TO WRK-SEQUENCIA
           ELSE
               COMPUTE WRK-SEQUENCIA = (CSEQ-AGPTO-CTA OF GFCTB0E7 + 1)
           END-IF.

      *----------------------------------------------------------------*
       2811-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR PAB                                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2820-GERAR-PAB                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP6-CAGPTO          TO CAGPTO-CTA        OF GFCTB0I1.
           MOVE GFCTP6-CTARIFA         TO CSERVC-TARIF      OF GFCTB0I1.
           MOVE GFCTP6-DATA-INICIO     TO DINIC-SUSP-COBR   OF GFCTB0I1.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0I1.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0I1.
           MOVE GFCTP6-CAGENCIA        TO CDEPDC            OF GFCTB0I1.
           MOVE GFCTP6-CPAB            TO CPOSTO-SERVC      OF GFCTB0I1.

           EXEC SQL
             INSERT INTO DB2PRD.TSUSP_GRP_PAB
                  (CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB0I1.CSERVC-TARIF,
                   :GFCTB0I1.DINIC-SUSP-COBR,
                   :GFCTB0I1.CAGPTO-CTA,
                   :GFCTB0I1.CSEQ-AGPTO-CTA,
                   :GFCTB0I1.CEMPR-INC,
                   :GFCTB0I1.CDEPDC,
                   :GFCTB0I1.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I1'      TO WRK-NOM-TAB
               MOVE 'TSUSP_GRP_PAB '   TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GEARAR AGENCIA                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2830-GERAR-AGENCIA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP6-CAGPTO          TO CAGPTO-CTA        OF GFCTB0E4.
           MOVE GFCTP6-CTARIFA         TO CSERVC-TARIF      OF GFCTB0E4.
           MOVE GFCTP6-DATA-INICIO     TO DINIC-SUSP-COBR   OF GFCTB0E4.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0E4.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0E4.
           MOVE GFCTP6-CAGENCIA        TO CDEPDC            OF GFCTB0E4.

           EXEC SQL
             INSERT INTO DB2PRD.SUSP_GRP_DEPDC
                  (CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC)
               VALUES
                  (:GFCTB0E4.CSERVC-TARIF,
                   :GFCTB0E4.DINIC-SUSP-COBR,
                   :GFCTB0E4.CAGPTO-CTA,
                   :GFCTB0E4.CSEQ-AGPTO-CTA,
                   :GFCTB0E4.CEMPR-INC,
                   :GFCTB0E4.CDEPDC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E4'      TO WRK-NOM-TAB
               MOVE 'SUSP_GRP_DEPDC'   TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR CLIENTE                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2840-GERAR-CLIENTE              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP6-CAGPTO          TO CAGPTO-CTA        OF GFCTB0E3.
           MOVE GFCTP6-CTARIFA         TO CSERVC-TARIF      OF GFCTB0E3.
           MOVE GFCTP6-DATA-INICIO     TO DINIC-SUSP-COBR   OF GFCTB0E3.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0E3.
ST2506*    M0VE GFCTP6-CGC-CPF         TO CCGC-CPF          OF GFCTB0E3.
ST2506*    M0VE GFCTP6-FILIAL          TO CFLIAL-CGC        OF GFCTB0E3.
ST2506*    M0VE GFCTP6-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0E3.
ST2506     MOVE GFCTP6-CGC-CPF         TO CCGC-CPF-ST       OF GFCTB0E3.
ST2506     MOVE GFCTP6-FILIAL          TO CFLIAL-CGC-ST     OF GFCTB0E3.
ST2506     MOVE GFCTP6-CONTROLE        TO CCTRL-CPF-CGC-ST  OF GFCTB0E3.

           EXEC SQL
             INSERT INTO DB2PRD.SUSP_GRP_CLI
                  (CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC,
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST)
               VALUES
                  (:GFCTB0E3.CSERVC-TARIF,
                   :GFCTB0E3.DINIC-SUSP-COBR,
                   :GFCTB0E3.CAGPTO-CTA,
                   :GFCTB0E3.CSEQ-AGPTO-CTA,
ST2506*            :GFCTB0E3.CCGC-CPF,
ST2506*            :GFCTB0E3.CFLIAL-CGC,
ST2506*            :GFCTB0E3.CCTRL-CPF-CGC,
ST2506             :GFCTB0E3.CCGC-CPF-ST,
ST2506             :GFCTB0E3.CFLIAL-CGC-ST,
ST2506             :GFCTB0E3.CCTRL-CPF-CGC-ST)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E3'      TO WRK-NOM-TAB
               MOVE 'SUSP_GRP_CLI'     TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2840-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR MUNICIPIO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2850-GERAR-MUNICIPIO            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP6-CAGPTO          TO CAGPTO-CTA        OF GFCTB0E5.
           MOVE GFCTP6-CTARIFA         TO CSERVC-TARIF      OF GFCTB0E5.
           MOVE GFCTP6-DATA-INICIO     TO DINIC-SUSP-COBR   OF GFCTB0E5.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0E5.
           MOVE GFCTP6-CMUNIC          TO CMUN-IBGE         OF GFCTB0E5.

           EXEC SQL
             INSERT INTO DB2PRD.SUSP_GRP_MUN
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CSEQ_AGPTO_CTA,
                   CMUN_IBGE)
               VALUES
                  (:GFCTB0E5.CSERVC-TARIF,
                   :GFCTB0E5.CAGPTO-CTA,
                   :GFCTB0E5.DINIC-SUSP-COBR,
                   :GFCTB0E5.CSEQ-AGPTO-CTA,
                   :GFCTB0E5.CMUN-IBGE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E5'      TO WRK-NOM-TAB
               MOVE 'SUSP_GRP_MUN'     TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2850-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GERAR UF                                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2860-GERAR-UF                   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTP6-CAGPTO          TO CAGPTO-CTA        OF GFCTB0E6.
           MOVE GFCTP6-CTARIFA         TO CSERVC-TARIF      OF GFCTB0E6.
           MOVE GFCTP6-DATA-INICIO     TO DINIC-SUSP-COBR   OF GFCTB0E6.
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA    OF GFCTB0E6.
           MOVE GFCTP6-CUF             TO CSGL-UF           OF GFCTB0E6.

           EXEC SQL
             INSERT INTO DB2PRD.SUSP_GRP_UF
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CSEQ_AGPTO_CTA,
                   CSGL_UF)
               VALUES
                  (:GFCTB0E6.CSERVC-TARIF,
                   :GFCTB0E6.CAGPTO-CTA,
                   :GFCTB0E6.DINIC-SUSP-COBR,
                   :GFCTB0E6.CSEQ-AGPTO-CTA,
                   :GFCTB0E6.CSGL-UF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0003'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E6'      TO WRK-NOM-TAB
               MOVE 'SUSP_GRP_UF '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE  0003              TO GFCTP7-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               MOVE SPACES             TO GFCTP7-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2860-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
