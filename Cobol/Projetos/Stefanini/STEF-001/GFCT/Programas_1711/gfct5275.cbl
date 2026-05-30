      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5275.
       AUTHOR.     VANI NUNES.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT5275                                     *
      *    PROGRAMADOR.:  VANI NUNES               - PROCKWORK - GP.50 *
      *    ANALISTA....:  FRANCISCO                - PROCKWORK - GP.50 *
      *    DATA........:  23/02/2006                                   *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  FLEXIBILIZACAO - LISTA  DE  CANCELAMENTO  DE *
      *       FLEXIBILIZACAO PARA AGENCIA/CONTA POR LOTE               *
      *                                                                *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2                                                         *
      *      TABLE                           INCLUDE/BOOK              *
      *          DB2PRD.PDIDO_FLEXZ_CTA         GFCTB0B5               *
      *          DB2PRD.PARM_DATA_PROCM         GFCTB0A1               *
      *          DB2PRD.TCTRL_FLEXZ_LOTE        GFCTB0M3               *
      *                                                                *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#WAK9 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#WALA - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5523 *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5522 *
      *    I#GFCTF9 - AREA DE COMUNICACAO - ENTRDA E SAIDA DO GFCT5515 *
      *    I#GFCT74 - AREA DE COMUNICACAO - ENTRDA E SAIDA DO GFCT5578 *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CBLTDLI  - OBTER E ENVIAR MENSAGENS PARA IMS                *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    GFCT5522 - VERIFICA DISPONIBILIDADE ON LINE                 *
      *    GFCT5508 - VERIFICA SE AG/CONTA SAO VALIDOS                 *
      *    GFCT5515 - OBTER IDENTIFICACAO DO SOLICITANTE DE FLEXIBILI- *
      *               ZACAO, POR AGENCIA/CONTA E USUARIO.              *
      *    GFCT5578 - CLONE DO GFCT5515                                *
      *    GFCT5516 - OBTER IDENTIFICACAO DO SOLICITANTE DE FLEXIBILI- *
      *               ZACAO, POR AGENCIA/CONTA E TARIFA.               *
      *                                                                *
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
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.
       77  IND-2                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE CAMPOS NULOS  ***'.
      *----------------------------------------------------------------*

       01  WRK-NLOTE-FLEXZ-CTA-NULL    PIC S9(004) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-DESPREZA                PIC  9(003)         VALUE ZEROS.
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.

       01  WRK-WALA-QTDE-TOT           PIC  9(006) COMP-3  VALUE ZEROS.
       01  WRK-WALA-QTDE-OCOR          PIC  9(003) COMP-3  VALUE ZEROS.
       01  WRK-FIM-CSR01               PIC  X(001)         VALUE 'N'.

       01  WRK-LOTE-INI                PIC S9(011) COMP-3  VALUE ZEROS.
       01  WRK-LOTE-FIM                PIC S9(011) COMP-3  VALUE ZEROS.

       01  WRK-CONJ-SERV-INI           PIC S9(003) COMP-3  VALUE ZEROS.
       01  WRK-CONJ-SERV-FIM           PIC S9(003) COMP-3  VALUE ZEROS.

       01  WRK-DPROCM-ATUAL            PIC  X(010)         VALUE SPACES.

       01  WRK-COUNT                   PIC S9(004) COMP    VALUE ZEROS.

       01  WRK-AUX-13-N                PIC  9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-13-N.
           05 WRK-AUX-13               PIC  9(013).
           05 FILLER                   REDEFINES WRK-AUX-13.
              10 FILLER                PIC  X(006).
              10 WRK-AUX-13-X.
                 15 WRK-AUX-13-7       PIC  9(007).

       01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL             PIC  9(003).

       01  WRK-AUX-9-V-S               PIC +9(009)V99      VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-9-V-S.
           05  FILLER                  PIC  X(004).
           05  WRK-AUX-9-V             PIC  9(006)V99.

       01  WRK-AUX-1-S                 PIC +9(001)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-1-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-1               PIC  9(001).

       01  WRK-AUX-2-S                 PIC +9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-2-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-2               PIC  9(002).

       01  WRK-AUX-3-V-S               PIC +9(003)V9(4)    VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-3-V-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-3-V             PIC  9(003)V9(4).

       01  WRK-AUX-3-S                 PIC +9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-3-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-3               PIC  9(003).

       01  WRK-AUX-5-S                 PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-5-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-5               PIC  9(005).

       01  WRK-AUX-11-S                PIC +9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-11-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-11              PIC  9(011).

       01  WRK-DPROCM-ATUAL-INV        PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-DPROCM-ATUAL-INV.
          05  WRK-ANO-PROCM-ATUAL-INV  PIC  9(004).
          05  WRK-MES-PROCM-ATUAL-INV  PIC  9(002).
          05  WRK-DIA-PROCM-ATUAL-INV  PIC  9(002).

       01  WRK-DATA-TT.
           03  WRK-ANO-TT              PIC  9(004)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-TT              PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-DIA-TT              PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(016)         VALUE SPACES.

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           03  FILLER                  PIC  X(025)         VALUE
               'ERRO DE ACESSO AO MODULO '.
           03  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5508 ***'.
      *----------------------------------------------------------------*

       01  WRK-5508-AREA-ENTRADA.
           05  WRK-5508-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5508-CDEPDC-CTA-CLI PIC  9(005)         VALUE ZEROS.
           05  WRK-5508-CNRO-CTA-CLI   PIC  9(007)         VALUE ZEROS.

       01  WRK-5508-AREA-SAIDA.
           05  WRK-5508-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5508-COD-SQL-ERRO   PIC  9(003)         VALUE ZEROS.
           05  WRK-5508-DESC-MSG       PIC  X(070)         VALUE SPACES.
           05  WRK-5508-DADOS-RETORNO.
            10  WRK-5508-IPSSOA-COPLT  PIC  X(070)         VALUE SPACES.

       01  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.

       01  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
            '*    AREA DO MODULO GFCT5578   *'.
      *----------------------------------------------------------------*

       COPY I#GFCT74.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5522 ***'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5523 ***'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B5
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0M3
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CURSORES ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR01-GFCTB0B5-JN CURSOR FOR
                   SELECT
                    T1.CJUNC_DEPDC,
                    T1.CCTA_CLI,
                    T1.CSERVC_TARIF,
                    T1.HIDTFD_FLEXZ_CTA,
                    T1.DINIC_FLEXZ_CTA,
                    T1.DFIM_FLEXZ_CTA,
                    T1.QFRANQ_FLEXZ_CTA,
                    T1.CPER_FRANQ_CTA,
                    T1.VNEGOC_FLEXZ_CTA,
                    T1.PDESC_FLEXZ_CTA,
                    T1.DDIA_AGNDA,
                    T1.QDIA_FLEXZ_CTA,
                    T1.CDEPDC_ORIGE_FLEXZ,
                    T1.CDEPDC_PAREC_FLEXZ,
                    T1.HSOLTC_FLEXZ,
                    T1.CSIT_SOLTC_FLEXZ,
                    T1.NLOTE_FLEXZ_CTA,
                    T1.CSERVC_TARIF
               FROM
                    DB2PRD.PDIDO_FLEXZ_CTA T1,
                    DB2PRD.TCTRL_FLEXZ_LOTE T2
               WHERE
                    T1.CJUNC_DEPDC            = :GFCTB0B5.CJUNC-DEPDC
               AND  T1.CCTA_CLI               = :GFCTB0B5.CCTA-CLI
               AND ((T1.CSIT_SOLTC_FLEXZ     IN (01, 10)
               AND   T1.CDEPDC_ORIGE_FLEXZ    =
                                          :GFCTB0B5.CDEPDC-ORIGE-FLEXZ)
                OR  (T1.CSIT_SOLTC_FLEXZ      = 02
               AND   T1.CDEPDC_PAREC_FLEXZ    =
                                          :GFCTB0B5.CDEPDC-PAREC-FLEXZ
               AND   T1.DINIC_FLEXZ_CTA       > :WRK-DPROCM-ATUAL)
                OR  (T1.CSIT_SOLTC_FLEXZ      = 07
               AND   ((T1.CDEPDC_PAREC_FLEXZ <> 0
               AND     T1.CDEPDC_PAREC_FLEXZ  =
                                        :GFCTB0B5.CDEPDC-PAREC-FLEXZ)
                OR    (T1.CDEPDC_PAREC_FLEXZ  = 0
               AND     T1.CDEPDC_ORIGE_FLEXZ  =
                                       :GFCTB0B5.CDEPDC-ORIGE-FLEXZ))))
               AND (T1.NLOTE_FLEXZ_CTA  BETWEEN   :WRK-LOTE-INI
                                           AND    :WRK-LOTE-FIM)
               AND  T1.NLOTE_FLEXZ_CTA        = T2.NLOTE_FLEXZ_CTA
               AND (T2.CCJTO_SERVC      BETWEEN  :WRK-CONJ-SERV-INI
                                            AND  :WRK-CONJ-SERV-FIM)

               ORDER BY T1.CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAK9.

       COPY GFCTWALA.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING WAK9-ENTRADA
                                             WALA-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL DO PROGRAMA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO DE VARIAVEIS.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WALA-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTG3-SAIDA
                                          GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE WALA-SAIDA
                      GFCTG2-ENTRADA
                      GFCTG3-SAIDA
                      GFCTFZ-ENTRADA
                      GFCTG1-SAIDA
                      GFCT0M-AREA-ERROS
                      IND-1.

           PERFORM 1100-INICIALIZAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-OBTER-DATA-DPROCM.

           PERFORM 1500-VALIDAR-AGENCIA-CONTA.

           PERFORM 1600-ACESSAR-GFCT5578

           MOVE GFCT74-DPTO           TO CDEPDC-ORIGE-FLEXZ OF GFCTB0B5
                                         CDEPDC-PAREC-FLEXZ OF GFCTB0B5.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZAR A AREA WALA-SAIDA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           MOVE +296                   TO WALA-LL.
           MOVE WAK9-ZZ                TO WALA-ZZ.
           MOVE WAK9-TRANSACAO         TO WALA-TRANSACAO.
           MOVE WAK9-FUNCAO            TO WALA-FUNCAO.

           IF  WAK9-QTDE-OCOR        NOT NUMERIC OR
               WAK9-QTDE-OCOR        EQUAL ZEROS
               MOVE ZEROS              TO WALA-QTDE-OCOR
           ELSE
               MOVE WAK9-QTDE-OCOR     TO WALA-QTDE-OCOR
           END-IF.

           MOVE  ZEROS                 TO WALA-QTDE-TOT-REG
           MOVE  WAK9-FUNC-BDSCO       TO WALA-FUNC-BDSCO.
           MOVE  WAK9-FILTRO           TO WALA-FILTRO.
           MOVE  WAK9-PONTEIRO         TO WALA-PONTEIRO.
           MOVE 'N'                    TO WALA-FIM.

           MOVE ZEROS                  TO WALA-ERRO
                                          WALA-COD-SQL-ERRO
                                          WALA-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO WALA-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR SITUACAO DO SISTEMA.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-SIST-DISPONIVEL  SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5522'             TO WRK-MODULO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE  WAK9-TRANSACAO        TO GFCTFZ-TRANSACAO.
           MOVE  WAK9-FUNCAO           TO GFCTFZ-FUNCAO.
           MOVE  ZEROS                 TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           CALL  WRK-MODULO         USING GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5275'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WALA-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO          EQUAL 9
               MOVE 'GFCT5275'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          WALA-COD-SQL-ERRO
               MOVE GFCTG1-ERRO        TO WALA-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO WALA-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO  (1:75)
                                       TO WALA-DESC-MSG-ERRO
                                          GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5275'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WALA-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTIR DADOS DE ENTRADA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAK9-TRANSACAO       EQUAL SPACES OR LOW-VALUES)   OR
              (WAK9-FUNCAO          EQUAL SPACES OR LOW-VALUES)   OR
              (WAK9-FUNC-BDSCO      EQUAL SPACES OR LOW-VALUES)   OR
              (WAK9-AGENCIA-FIL       NOT NUMERIC)                OR
              (WAK9-AGENCIA-FIL     EQUAL ZEROS)                  OR
              (WAK9-CONTA-FIL         NOT NUMERIC)                OR
              (WAK9-CONTA-FIL       EQUAL ZEROS)                  OR
              (WAK9-CONJ-SERV-FIL     NOT NUMERIC)                OR
              (WAK9-COD-LOTE-FIL      NOT NUMERIC)                OR
              (WAK9-CANC-TODAS-FIL  EQUAL SPACES OR LOW-VALUES)   OR
              (WAK9-AGENCIA-PONT      NOT NUMERIC)                OR
              (WAK9-CONTA-PONT        NOT NUMERIC)                OR
              (WAK9-CONJ-SERV-PONT    NOT NUMERIC)                OR
              (WAK9-COD-LOTE-PONT     NOT NUMERIC)
               MOVE 'S'                TO WALA-FIM
               MOVE 1                  TO WALA-ERRO
               MOVE ZEROS              TO WALA-COD-SQL-ERRO
               MOVE SPACES             TO WALA-DESC-MSG-ERRO
               MOVE 0001               TO WALA-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WALA-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DESCRICAO DA MENSAGEM.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAK9-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAK9-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAK9-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTG2-ENTRADA
                                          GFCTG3-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5275'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WALA-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO          EQUAL 9
               MOVE GFCTG3-ERRO        TO WALA-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5275'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO          EQUAL 1
               MOVE 'S'                TO WALA-FIM
               MOVE GFCTG3-ERRO        TO WALA-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WALA-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WALA-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WALA-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DATA DO PROCESSAMENTO.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-OBTER-DATA-DPROCM          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
               SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
               INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
               FROM DB2PRD.PARM_DATA_PROCM
               WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'GFCT5275'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO WALA-ERRO
               MOVE  SQLCODE           TO WRK-SQLCODE
               MOVE  WRK-COD-SQL       TO WALA-COD-SQL-ERRO
               MOVE  SPACES            TO WALA-DESC-MSG-ERRO
               MOVE  0010              TO WALA-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  DPROCM-ATUAL          TO WRK-DATA-DB2.
           MOVE  WRK-ANO-DB2           TO WRK-ANO-PROCM-ATUAL-INV.
           MOVE  WRK-MES-DB2           TO WRK-MES-PROCM-ATUAL-INV.
           MOVE  WRK-DIA-DB2           TO WRK-DIA-PROCM-ATUAL-INV.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONCATENAR MENSAGEM.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1410-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO WALA-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR AGENCIA E CONTA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-VALIDAR-AGENCIA-CONTA      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5508-AMBIENTE.
           MOVE  WAK9-AGENCIA-FIL      TO WRK-5508-CDEPDC-CTA-CLI.
           MOVE  WAK9-CONTA-FIL        TO WRK-AUX-13-N
           MOVE  WRK-AUX-13-7          TO WRK-5508-CNRO-CTA-CLI.
           MOVE 'GFCT5508'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING WRK-5508-AREA-ENTRADA
                                          WRK-5508-AREA-SAIDA
                                          WRK-IO-PCB
                                          WRK-ALT-PCB.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5275'         TO GFCT0M-PROGRAMA
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          WALA-DESC-MSG-ERRO
               MOVE 9                  TO WALA-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5508-COD-RETORNO       EQUAL 02
               MOVE  'S'               TO WALA-FIM
               MOVE  1                 TO WALA-ERRO
               MOVE  ZEROS             TO WALA-COD-SQL-ERRO
               MOVE  0221              TO WALA-COD-MSG-ERRO
               MOVE '0221'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO WALA-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSAR MODULO GFCT5578 - GESTOR LOGADO.           *
      ******************************************************************
      *----------------------------------------------------------------*
       1600-ACESSAR-GFCT5578           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCT74-GFCT5578.

           MOVE WAK9-AGENCIA-FIL       TO GFCT74-AGENCIA.
           MOVE WAK9-CONTA-FIL         TO WRK-AUX-13-N
           MOVE WRK-AUX-13-7           TO GFCT74-CONTA.
           MOVE WAK9-FUNC-BDSCO        TO GFCT74-COD-FUNC.

           MOVE 'GFCT5578'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCT74-GFCT5578.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5275'         TO GFCT0M-PROGRAMA
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          WALA-DESC-MSG-ERRO
               MOVE 9                  TO WALA-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT74-COD-RETORNO      NOT EQUAL ZEROS AND 10
               MOVE 'S'                TO WALA-FIM
               MOVE 1                  TO WALA-ERRO

               PERFORM 1610-VERIFICAR-CODIGO

               PERFORM 1310-OBTER-DESC-MSG

               MOVE  GFCTG3-DESC-MSG   TO WALA-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
    *** ROTINA PARA VERIFICAR CODIGO DE RETORNO DO MODULO GFCT5578.    *
      ******************************************************************
      *----------------------------------------------------------------*
       1610-VERIFICAR-CODIGO           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT74-COD-RETORNO
               WHEN 02
                 MOVE 0435             TO WALA-COD-MSG-ERRO
                 MOVE '0435'           TO GFCTG2-COD-MSG
               WHEN 03
                 MOVE 0436             TO WALA-COD-MSG-ERRO
                 MOVE '0436'           TO GFCTG2-COD-MSG
               WHEN 04
                 MOVE 0437             TO WALA-COD-MSG-ERRO
                 MOVE '0437'           TO GFCTG2-COD-MSG
               WHEN 06
                 MOVE 0438             TO WALA-COD-MSG-ERRO
                 MOVE '0438'           TO GFCTG2-COD-MSG
               WHEN 07
                 MOVE 0439             TO WALA-COD-MSG-ERRO
                 MOVE '0439'           TO GFCTG2-COD-MSG
               WHEN 08
                 MOVE 0440             TO WALA-COD-MSG-ERRO
                 MOVE '0440'           TO GFCTG2-COD-MSG
               WHEN 09
                 MOVE 0441             TO WALA-COD-MSG-ERRO
                 MOVE '0441'           TO GFCTG2-COD-MSG
               WHEN 11
                 MOVE 0810             TO WALA-COD-MSG-ERRO
                 MOVE '0810'           TO GFCTG2-COD-MSG
           END-EVALUATE.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAMENTO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE  WAK9-AGENCIA-FIL      TO CJUNC-DEPDC   OF GFCTB0B5.

           MOVE  WAK9-CONTA-FIL        TO CCTA-CLI      OF GFCTB0B5.

           IF  WAK9-CONJ-SERV-FIL        EQUAL ZEROS
               MOVE ZEROS              TO WRK-CONJ-SERV-INI
               MOVE 999                TO WRK-CONJ-SERV-FIM
           ELSE
               MOVE WAK9-CONJ-SERV-FIL TO WRK-CONJ-SERV-INI
                                          WRK-CONJ-SERV-FIM
           END-IF.

           IF  WAK9-COD-LOTE-FIL         EQUAL ZEROS
               MOVE ZEROS              TO WRK-LOTE-INI
               MOVE 99999999999        TO WRK-LOTE-FIM
           ELSE
               MOVE WAK9-COD-LOTE-FIL  TO WRK-LOTE-INI
                                          WRK-LOTE-FIM
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DPROCM-ATUAL.

           PERFORM 2100-PROCESSAR-SELECT-COUNT.

           IF  WRK-COUNT               GREATER 150
               MOVE  ZEROS             TO WALA-ERRO
                                          WALA-COD-SQL-ERRO
               MOVE 0662               TO WALA-COD-MSG-ERRO
               MOVE '0662'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO WALA-DESC-MSG-ERRO
               MOVE 'S'                TO WRK-FIM-CSR01
               GO                      TO 2000-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR01

           PERFORM 2200-ABRIR-CSR01.

           PERFORM 2300-LER-CSR01

           MOVE  WAK9-QTDE-OCOR         TO WRK-DESPREZA

           PERFORM 2300-LER-CSR01
           VARYING IND-2               FROM 1 BY 1
             UNTIL IND-2               GREATER WRK-DESPREZA
                OR WRK-FIM-CSR01    EQUAL 'S'.

           PERFORM 2400-PROCESSAR-CSR01
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER 7  OR
                   WRK-FIM-CSR01       EQUAL 'S'.

           PERFORM 2500-FECHAR-CSR01.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR QTDE DE REGISTROS ENCONTRADOS            *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-SELECT-COUNT     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT COUNT(*)
               INTO   :WRK-COUNT
               FROM
                      DB2PRD.PDIDO_FLEXZ_CTA  T1,
                      DB2PRD.TCTRL_FLEXZ_LOTE T2
               WHERE
                    T1.CJUNC_DEPDC            = :GFCTB0B5.CJUNC-DEPDC
               AND  T1.CCTA_CLI               = :GFCTB0B5.CCTA-CLI
               AND ((T1.CSIT_SOLTC_FLEXZ      = 01
               AND   T1.CDEPDC_ORIGE_FLEXZ    =
                                          :GFCTB0B5.CDEPDC-ORIGE-FLEXZ)
                OR  (T1.CSIT_SOLTC_FLEXZ      = 02
               AND   T1.CDEPDC_PAREC_FLEXZ    =
                                          :GFCTB0B5.CDEPDC-PAREC-FLEXZ
               AND   T1.DINIC_FLEXZ_CTA       > :WRK-DPROCM-ATUAL)
                OR  (T1.CSIT_SOLTC_FLEXZ      = 07
               AND   ((T1.CDEPDC_PAREC_FLEXZ <> 0
               AND     T1.CDEPDC_PAREC_FLEXZ  =
                                        :GFCTB0B5.CDEPDC-PAREC-FLEXZ)
                OR    (T1.CDEPDC_PAREC_FLEXZ  = 0
               AND     T1.CDEPDC_ORIGE_FLEXZ  =
                                       :GFCTB0B5.CDEPDC-ORIGE-FLEXZ))))
               AND (T1.NLOTE_FLEXZ_CTA  BETWEEN   :WRK-LOTE-INI
                                        AND       :WRK-LOTE-FIM)
               AND  T1.NLOTE_FLEXZ_CTA        = T2.NLOTE_FLEXZ_CTA
               AND (T2.CCJTO_SERVC      BETWEEN  :WRK-CONJ-SERV-INI
                                        AND      :WRK-CONJ-SERV-FIM)

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0B5'      TO WRK-NOME-TAB
               MOVE 'GFCT5275'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO WALA-ERRO
               MOVE  SQLCODE           TO WRK-SQLCODE
               MOVE  WRK-COD-SQL       TO WALA-COD-SQL-ERRO
               MOVE  SPACES            TO WALA-DESC-MSG-ERRO
               MOVE  0010              TO WALA-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABERTURA DE CURSORES.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ABRIR-CSR01                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB0B5-JN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0B5'      TO WRK-NOME-TAB
               MOVE 'GFCT5275'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO WALA-ERRO
               MOVE  SQLCODE           TO WRK-SQLCODE
               MOVE  WRK-COD-SQL       TO WALA-COD-SQL-ERRO
               MOVE  SPACES            TO WALA-DESC-MSG-ERRO
               MOVE  0010              TO WALA-COD-MSG-ERRO
               MOVE '0010'             TO  GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LEITURA DE CURSORES.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CSR01                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-GFCTB0B5-JN
                INTO
                     :GFCTB0B5.CJUNC-DEPDC,
                     :GFCTB0B5.CCTA-CLI,
                     :GFCTB0B5.CSERVC-TARIF,
                     :GFCTB0B5.HIDTFD-FLEXZ-CTA,
                     :GFCTB0B5.DINIC-FLEXZ-CTA,
                     :GFCTB0B5.DFIM-FLEXZ-CTA,
                     :GFCTB0B5.QFRANQ-FLEXZ-CTA,
                     :GFCTB0B5.CPER-FRANQ-CTA,
                     :GFCTB0B5.VNEGOC-FLEXZ-CTA,
                     :GFCTB0B5.PDESC-FLEXZ-CTA,
                     :GFCTB0B5.DDIA-AGNDA,
                     :GFCTB0B5.QDIA-FLEXZ-CTA,
                     :GFCTB0B5.CDEPDC-ORIGE-FLEXZ,
                     :GFCTB0B5.CDEPDC-PAREC-FLEXZ,
                     :GFCTB0B5.HSOLTC-FLEXZ,
                     :GFCTB0B5.CSIT-SOLTC-FLEXZ,
                     :GFCTB0B5.NLOTE-FLEXZ-CTA
                          :WRK-NLOTE-FLEXZ-CTA-NULL,
                     :GFCTB0B5.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0B5'      TO WRK-NOME-TAB
               MOVE 'GFCT5275'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO WALA-ERRO
               MOVE  SQLCODE           TO WRK-SQLCODE
               MOVE  WRK-COD-SQL       TO WALA-COD-SQL-ERRO
               MOVE  SPACES            TO WALA-DESC-MSG-ERRO
               MOVE  0006              TO WALA-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WALA-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL +100
               MOVE 'S'                TO WALA-FIM
                                          WRK-FIM-CSR01
               IF  IND-1            EQUAL ZEROS
                   MOVE  ZEROS         TO WALA-ERRO
                                          WALA-COD-SQL-ERRO
                   MOVE  1673          TO WALA-COD-MSG-ERRO
                   MOVE '1673'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO WALA-DESC-MSG-ERRO
               END-IF
           ELSE
               IF  WRK-NLOTE-FLEXZ-CTA-NULL
                                       NOT EQUAL ZEROS
                   MOVE ZEROS          TO NLOTE-FLEXZ-CTA OF GFCTB0B5
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR CSR01                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-CSR01            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-MOVER-CAMPOS

           PERFORM 2300-LER-CSR01.

           IF  WRK-FIM-CSR01     EQUAL 'N'
               IF  IND-1         EQUAL 07
                   MOVE  'S'           TO WRK-FIM-CSR01
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA MOVIMENTACAO DE CAMPOS PARA INC DE SAIDA.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

           MOVE WALA-QTDE-OCOR         TO WRK-WALA-QTDE-OCOR
           MOVE WALA-QTDE-TOT-REG      TO WRK-WALA-QTDE-TOT

           ADD 1                       TO WRK-WALA-QTDE-TOT
                                          WRK-WALA-QTDE-OCOR

           MOVE WRK-WALA-QTDE-OCOR     TO WALA-QTDE-OCOR
           MOVE WRK-WALA-QTDE-TOT      TO WALA-QTDE-TOT-REG

           ADD +102                    TO WALA-LL.

           MOVE  CSERVC-TARIF          OF GFCTB0B5
                                       TO WRK-AUX-5-S
           MOVE  WRK-AUX-5             TO WALA-COD-TARIFA(IND-1).
           MOVE  HIDTFD-FLEXZ-CTA      OF GFCTB0B5
                                       TO WALA-TIMESTAMP(IND-1).
           MOVE  DINIC-FLEXZ-CTA       OF GFCTB0B5
                                       TO WALA-INIC-VIG-FLEX(IND-1).
           MOVE  DFIM-FLEXZ-CTA        OF GFCTB0B5
                                       TO WALA-FIM-VIG-FLEX(IND-1).
           MOVE  QFRANQ-FLEXZ-CTA      OF GFCTB0B5
                                       TO WRK-AUX-3-S
           MOVE  WRK-AUX-3             TO WALA-FRANQUIA(IND-1).
           MOVE  CPER-FRANQ-CTA        OF GFCTB0B5
                                       TO WRK-AUX-1-S
           MOVE  WRK-AUX-1             TO WALA-PERIODO(IND-1).
           MOVE  VNEGOC-FLEXZ-CTA      OF GFCTB0B5
                                       TO WRK-AUX-9-V-S
           MOVE  WRK-AUX-9-V           TO WALA-VR-NEGOCIADO(IND-1).
           MOVE  PDESC-FLEXZ-CTA       OF GFCTB0B5
                                       TO WRK-AUX-3-V-S
           MOVE  WRK-AUX-3-V           TO WALA-PERC-DESCTO(IND-1).
           MOVE  DDIA-AGNDA            OF GFCTB0B5
                                       TO WRK-AUX-2-S
           MOVE  WRK-AUX-2             TO WALA-DIA-AGEND(IND-1).
           MOVE  QDIA-FLEXZ-CTA        OF GFCTB0B5
                                       TO WRK-AUX-2-S
           MOVE  WRK-AUX-2             TO WALA-DIAS-RETENCAO(IND-1).
           MOVE  CSIT-SOLTC-FLEXZ      OF GFCTB0B5
                                       TO WRK-AUX-2-S
           MOVE  WRK-AUX-2             TO WALA-SITUACAO-FLEX(IND-1).
           MOVE  CDEPDC-ORIGE-FLEXZ    OF GFCTB0B5
                                       TO WRK-AUX-5-S
           MOVE  WRK-AUX-5             TO WALA-JUNCAO-SOLIC(IND-1).
           MOVE  HSOLTC-FLEXZ          OF GFCTB0B5
                                       TO WRK-DATA-TT
           MOVE  WRK-DIA-TT            TO WRK-DIA-DB2
           MOVE  WRK-MES-TT            TO WRK-MES-DB2
           MOVE  WRK-ANO-TT            TO WRK-ANO-DB2
           MOVE  WRK-DATA-DB2          TO WALA-DT-SOLICT(IND-1).
           MOVE  NLOTE-FLEXZ-CTA       OF GFCTB0B5
                                       TO WRK-AUX-11-S
           MOVE  WRK-AUX-11            TO WALA-NRO-LOTE(IND-1).

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-FECHAR-CSR01               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0B5-JN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0B5'      TO WRK-NOME-TAB
               MOVE 'GFCT5275'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO WALA-ERRO
               MOVE  SQLCODE           TO WRK-SQLCODE
               MOVE  WRK-COD-SQL       TO WALA-COD-SQL-ERRO
               MOVE  SPACES            TO WALA-DESC-MSG-ERRO
               MOVE  0011              TO WALA-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
