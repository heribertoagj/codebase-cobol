      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0144.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0144                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK - GP.50  *
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK - GP.50  *
      *    DATA........:   05/03/2009                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      LISTA DE REGISTRO DE EXCLUSAO MASSIVA - DEFERIMENTO.      *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.TCANCT_ADSAO_AGPTO              GFCTB0L0          *
      *       DB2PRD.TCANCT_ADSAO_EMPR               GFCTB0L1          *
      *       DB2PRD.TCANCT_ADSAO_FORNC              GFCTB0L2          *
      *       DB2PRD.TCANCT_ADSAO_POSTO              GFCTB0L3          *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
      *      GFCT5109 - OBTER DESCRICAO DO PAB                         *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7148                                     *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT0144  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC 9(05)   COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUX-9-S                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-9-S.
           05 WRK-AUX-9                PIC  9(009).
           05 FILLER                   REDEFINES WRK-AUX-9.
              10 FILLER                PIC  X(006).
              10 WRK-AUX-9-X.
                 15 WRK-AUX-9-3        PIC  9(003).

ST2506*01  WRK-AUX-CNPJ                PIC +9(009)         VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES           WRK-AUX-CNPJ.
ST2506*    05  FILLER                  PIC  X(001).
ST2506*    05  WRK-AUX-PJ              PIC  9(009).
ST2506 01  WRK-CNPJ-CGC                PIC X(009)         VALUE SPACES.

       01  WRK-AUX-5-S                 PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-5-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-5               PIC  9(005).

ST2506 01  WRK-AUX-X-4                 PIC  X(004)         VALUE SPACES.

       01  WRK-AUX-2-S                 PIC +9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-2-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-2               PIC  9(002).

       01  WRK-AUX-13-S                PIC +9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-13-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-13              PIC  9(013).

       01  WRK-DESPREZA                PIC  9(005)  COMP-3 VALUE ZEROS.
       01  WRK-ACHO                    PIC  X(001)         VALUE 'N'.
       01  WRK-WAFK-QTDE-TOT           PIC  9(006) COMP-3  VALUE ZEROS.

       01  WRK-WAFK-QTDE-OCOR          PIC  9(003) COMP-3  VALUE ZEROS.
       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-TARIFA                  PIC S9(05)          VALUE ZEROS.
       01  WRK-TARIFA-INI              PIC S9(05)  COMP-3  VALUE ZEROS.
       01  WRK-TARIFA-FIM              PIC S9(05)  COMP-3  VALUE ZEROS.
       01  WRK-LER-CSR                 PIC  9(01)          VALUE ZEROS.

       01  WRK-TIMESTAMP               PIC X(26)           VALUE
           '0001-01-01-01.01.01.000001'.

       01  WRK-HINCL.
           03  WRK-DATA.
               05  WRK-ANO             PIC 9(04)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-MES             PIC 9(02)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-DIA             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(16)           VALUE SPACES.

       01  WRK-DATA-AUX.
           03  WRK-DIA-AUX             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-MES-AUX             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-ANO-AUX             PIC 9(04)           VALUE ZEROS.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MSG05-MODULO        PIC X(08)           VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(041)         VALUE SPACES.

       01  WRK-MSG002                  PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5523'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5522'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5090   *'.
      *----------------------------------------------------------------*
       COPY 'I#GFCTGB'.
       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L1
           END-EXEC.


           EXEC SQL
                INCLUDE GFCTB0L2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

      *--> '*     AREAS DECLARE CURSOR     *'

           EXEC SQL
               DECLARE CSR01-GFCTB0L0 CURSOR WITH HOLD FOR
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       HINCL_REG,
                       CFUNC_SOLTC_CANCT,
                       CFUNC_MANUT,
                       CSIT_SOLTC_CANCT,
                       QPRVIO_CANCT_ADSAO,
                       QADSAO_CANCD,
                       HEXCUC_CALC_PRVIO,
                       HEXCUC_PROCM,
                       HMANUT_REG,
                       RJUSTF_SOLTC_CANCT,
                       RJUSTF_PAREC_CANCT
               FROM    DB2PRD.TCANCT_ADSAO_AGPTO
               WHERE   CSIT_SOLTC_CANCT   = :GFCTB0L0.CSIT-SOLTC-CANCT
                 AND   CAGPTO_CTA         = :GFCTB0L0.CAGPTO-CTA
                 AND ((CSERVC_TARIF BETWEEN :WRK-TARIFA-INI AND
                                            :WRK-TARIFA-FIM
                 AND   HINCL_REG         >= :GFCTB0L0.HINCL-REG)
                  OR  (CSERVC_TARIF      >  :GFCTB0L0.CSERVC-TARIF))
            ORDER BY   CSERVC_TARIF,
                       HINCL_REG
           END-EXEC.

           EXEC SQL
               DECLARE CSR02-GFCTB0L0 CURSOR WITH HOLD FOR
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       HINCL_REG,
                       CFUNC_SOLTC_CANCT,
                       CFUNC_MANUT,
                       CSIT_SOLTC_CANCT,
                       QPRVIO_CANCT_ADSAO,
                       QADSAO_CANCD,
                       HEXCUC_CALC_PRVIO,
                       HEXCUC_PROCM,
                       HMANUT_REG,
                       RJUSTF_SOLTC_CANCT,
                       RJUSTF_PAREC_CANCT
               FROM    DB2PRD.TCANCT_ADSAO_AGPTO
               WHERE   CSIT_SOLTC_CANCT   = :GFCTB0L0.CSIT-SOLTC-CANCT
                 AND   CAGPTO_CTA         = :GFCTB0L0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0L0.CSERVC-TARIF
                 AND   HINCL_REG         >= :GFCTB0L0.HINCL-REG
            ORDER BY   CSERVC_TARIF,
                       HINCL_REG
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT0144   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       COPY GFCTWAFJ.
       COPY GFCTWAFK.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING WAFJ-ENTRADA
                                             WAFK-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAFK-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  WAFK-SAIDA
                                       GFCT0M-AREA-ERROS
                                       SQLCA.

           MOVE 163                    TO WAFK-LL
           MOVE WAFJ-TRANSACAO         TO WAFK-TRANSACAO
           MOVE WAFJ-FUNCAO            TO WAFK-FUNCAO
           MOVE WAFJ-FUNC-BDSCO        TO WAFK-FUNC-BDSCO
           MOVE WAFJ-FILTRO            TO WAFK-FILTRO
           MOVE WAFJ-PONTEIRO          TO WAFK-PONTEIRO
           MOVE ZEROS                  TO WAFK-ERRO
                                          WAFK-COD-SQL-ERRO
                                          WAFK-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.
           MOVE 'N'                    TO WAFK-FIM
                                          WRK-ACHO
                                          WRK-FINALIZAR.

           MOVE ZEROS                  TO WAFK-QTDE-TOT-REG
                                          WRK-WAFK-QTDE-TOT
           MOVE WAFJ-QTDE-OCOR         TO WAFK-QTDE-OCOR

           PERFORM 1100-OBTER-DESC-MSG.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCTG3-SAIDA

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAFJ-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAFJ-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAFJ-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.


           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAFK-DESC-MSG-ERRO
               MOVE 9                  TO WAFK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0144'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAFK-DESC-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAFK-COD-SQL-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAFK-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0144'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO        TO WAFK-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE GFCTG3-ERRO        TO WAFK-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAFK-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAFK-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAFK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTG3-DESC-MSG        TO WAFK-DESC-MSG-ERRO
                                         (1: LENGTH OF GFCTG3-DESC-MSG)
                                          GFCT0M-TEXTO
                                         (1: LENGTH OF GFCTG3-DESC-MSG).

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-CONSISTIR-CAMPOS.

           MOVE 02                     TO CSIT-SOLTC-CANCT OF GFCTB0L0
           MOVE WAFJ-FLT-AGPTO         TO CAGPTO-CTA       OF GFCTB0L0

           IF  WAFJ-HINCL(1)        EQUAL SPACES OR LOW-VALUES
               MOVE WRK-TIMESTAMP      TO HINCL-REG        OF GFCTB0L0
           ELSE
               MOVE WAFJ-HINCL(1)      TO HINCL-REG        OF GFCTB0L0
           END-IF.

           IF  WAFJ-FLT-TARIFA      EQUAL ZEROS
               MOVE  1                 TO WRK-LER-CSR
               MOVE  ZEROS             TO WRK-TARIFA-INI
                                          CSERVC-TARIF OF GFCTB0L0
               MOVE  99999             TO WRK-TARIFA-FIM
           ELSE
               MOVE  2                 TO WRK-LER-CSR
               MOVE  WAFJ-FLT-TARIFA   TO CSERVC-TARIF OF GFCTB0L0
           END-IF.

           IF  WRK-LER-CSR          EQUAL 1
               PERFORM 2200-TRATAR-CURSOR-1
           ELSE
               PERFORM 2400-TRATAR-CURSOR-2
           END-IF.

           IF  WRK-ACHO             EQUAL 'N'
               MOVE 1                  TO WAFK-ERRO
               MOVE ZEROS              TO WAFK-COD-SQL-ERRO
               MOVE 'S'                TO WAFK-FIM
               PERFORM 1100-OBTER-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-VERIFICAR-ONLINE.

           IF (WAFJ-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (WAFJ-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (WAFJ-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (WAFJ-FLT-AGPTO        NOT NUMERIC)                OR
              (WAFJ-FLT-AGPTO        EQUAL ZEROS)                OR
              (WAFJ-FLT-TARIFA       NOT NUMERIC)
               MOVE 1                  TO WAFK-ERRO
               MOVE ZEROS              TO WAFK-COD-SQL-ERRO
               MOVE 0001               TO WAFK-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAFK-FIM
               PERFORM 1100-OBTER-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +101                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WAFJ-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAFJ-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAFJ-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAFK-DESC-MSG-ERRO
               MOVE 9                  TO WAFK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAFK-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO

                                       TO WAFK-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAFK-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WAFK-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAFK-ERRO
               MOVE SPACES             TO WAFK-DESC-MSG-ERRO
               MOVE WRK-MSG002         TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-TRATAR-CURSOR-1            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-ABRIR-CSR01-GFCTB0L0

           MOVE WAFJ-QTDE-OCOR         TO WRK-DESPREZA

           PERFORM 2220-LER-CSR01-GFCTB0L0

           PERFORM 2220-LER-CSR01-GFCTB0L0  VARYING  IND-1
                   FROM 1              BY 1
                   UNTIL IND-1         GREATER WRK-DESPREZA OR
                   WRK-FINALIZAR       EQUAL 'S'.

           MOVE ZEROS                  TO IND-1

           PERFORM 2230-PROCESSAR-CURSOR-1
                                       UNTIL WRK-FINALIZAR EQUAL 'S'

           PERFORM 2240-FECHAR-CSR01-GFCTB0L0.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-ABRIR-CSR01-GFCTB0L0       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB0L0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')

               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAFK-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L0'         TO WRK-NOME-TAB
               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2211-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0144'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO WAFK-ERRO.

           PERFORM 1100-OBTER-DESC-MSG.

           MOVE SPACES                 TO WAFK-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG   ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO WAFK-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-LER-CSR01-GFCTB0L0         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR01-GFCTB0L0
               INTO    :GFCTB0L0.CAGPTO-CTA,
                       :GFCTB0L0.CSERVC-TARIF,
                       :GFCTB0L0.HINCL-REG,
                       :GFCTB0L0.CFUNC-SOLTC-CANCT,
                       :GFCTB0L0.CFUNC-MANUT,
                       :GFCTB0L0.CSIT-SOLTC-CANCT,
                       :GFCTB0L0.QPRVIO-CANCT-ADSAO,
                       :GFCTB0L0.QADSAO-CANCD,
                       :GFCTB0L0.HEXCUC-CALC-PRVIO,
                       :GFCTB0L0.HEXCUC-PROCM,
                       :GFCTB0L0.HMANUT-REG,
                       :GFCTB0L0.RJUSTF-SOLTC-CANCT,
                       :GFCTB0L0.RJUSTF-PAREC-CANCT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')

               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAFK-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L0'         TO WRK-NOME-TAB
               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          WAFK-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO WAFK-ERRO
                   MOVE ZEROS          TO WAFK-COD-SQL-ERRO

                   MOVE 0484           TO WAFK-COD-MSG-ERRO
                   MOVE '0484'         TO GFCTG2-COD-MSG
                   PERFORM 1100-OBTER-DESC-MSG
                   MOVE 'S'            TO WAFK-FIM
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-PROCESSAR-CURSOR-1         SECTION.
      *----------------------------------------------------------------*

           IF  CSERVC-TARIF OF GFCTB0L0
                                       NOT EQUAL WRK-TARIFA
               PERFORM 2231-ACESSAR-GFCTB0D8

               MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO WRK-TARIFA
           END-IF.

           ADD 1                       TO IND-1
           PERFORM 2300-MOVER-CAMPOS

           PERFORM 2220-LER-CSR01-GFCTB0L0

           IF  IND-1                   EQUAL 7
               IF  WRK-FINALIZAR       NOT EQUAL 'S'
                   MOVE 'S'            TO WRK-FINALIZAR
                   MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO WRK-AUX-5-S
                   MOVE WRK-AUX-5      TO WAFK-PNT-TARIFA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2231-ACESSAR-GFCTB0D8           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  CDEPDC,
                       RSERVC_TARIF_REDZD
               INTO    :GFCTB0D8.CDEPDC,
                       :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF  = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB

               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAFK-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-FECHAR-CSR01-GFCTB0L0      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0L0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAFK-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L0'         TO WRK-NOME-TAB
               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*

       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

           ADD 121                     TO WAFK-LL.

           EVALUATE WAFJ-FLT-AGPTO
              WHEN 15
                  PERFORM 2310-OBTER-PAB
              WHEN 17
                  PERFORM 2310-OBTER-PAB
              WHEN 20
                  PERFORM 2310-OBTER-PAB
              WHEN 16
                  PERFORM 2320-OBTER-EMPRESA
              WHEN OTHER
                  PERFORM 2330-OBTER-FORNEC
           END-EVALUATE.

           MOVE 'S'                    TO WRK-ACHO
           MOVE CSERVC-TARIF           OF GFCTB0L0
                                       TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO WAFK-PACOTE(IND-1)
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8 (1:20)
                                       TO WAFK-NOME-PACOTE(IND-1)
           MOVE HINCL-REG OF GFCTB0L0  TO WAFK-HINCL(IND-1)
                                          WRK-HINCL
           MOVE WRK-ANO                TO WRK-ANO-AUX
           MOVE WRK-MES                TO WRK-MES-AUX
           MOVE WRK-DIA                TO WRK-DIA-AUX
           MOVE WRK-DATA-AUX           TO WAFK-DATA-SOLIC(IND-1)

           MOVE WAFK-QTDE-OCOR         TO WRK-WAFK-QTDE-OCOR
           ADD 1                       TO WRK-WAFK-QTDE-OCOR
           MOVE WRK-WAFK-QTDE-OCOR     TO WAFK-QTDE-OCOR

           MOVE WAFK-QTDE-TOT-REG      TO WRK-WAFK-QTDE-TOT
           ADD 1                       TO WRK-WAFK-QTDE-TOT
           MOVE WRK-WAFK-QTDE-TOT      TO WAFK-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-OBTER-PAB                  SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA      OF GFCTB0L3.
           MOVE CSERVC-TARIF           OF GFCTB0L0
                                       TO CSERVC-TARIF    OF GFCTB0L3.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG       OF GFCTB0L3.

           EXEC SQL
               SELECT  CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO    :GFCTB0L3.CEMPR-INC,
                       :GFCTB0L3.CDEPDC,
                       :GFCTB0L3.CPOSTO-SERVC

               FROM    DB2PRD.TCANCT_ADSAO_POSTO
               WHERE   CAGPTO_CTA         = :GFCTB0L3.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0L3.CSERVC-TARIF
                 AND   HINCL_REG          = :GFCTB0L3.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAFK-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L3'         TO WRK-NOME-TAB
               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL +100
               EVALUATE WAFJ-FLT-AGPTO
                  WHEN 15
                      MOVE 1571        TO WAFK-COD-MSG-ERRO
                      MOVE '1571'      TO GFCTG2-COD-MSG
                  WHEN 17
                      MOVE 1572        TO WAFK-COD-MSG-ERRO
                      MOVE '1572'      TO GFCTG2-COD-MSG
                  WHEN 20
                      MOVE 1573        TO WAFK-COD-MSG-ERRO
                      MOVE '1573'      TO GFCTG2-COD-MSG
               END-EVALUATE
               MOVE ZEROS              TO WAFK-COD-SQL-ERRO
           ELSE
               MOVE CDEPDC OF GFCTB0L3 TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAFK-AGENCIA(IND-1)
               MOVE CPOSTO-SERVC OF GFCTB0L3
                                           TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAFK-POSTO(IND-1)

               IF  WAFJ-FLT-AGPTO   EQUAL 15
                   PERFORM 2311-OBTER-DESC-PAB
               ELSE
                   PERFORM 2312-OBTER-DESC-CBANC
               END-IF

               MOVE GFCTGC-DESC-POSTO(1) TO WAFK-NOME-POSTO(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2311-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.

           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE WAFJ-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAFJ-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE WAFJ-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC OF GFCTB0L3     TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTGB-COD-DEPDC
           MOVE CPOSTO-SERVC OF GFCTB0L3
                                       TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTGB-COD-POSTO

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 9                  TO WAFK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0144'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO            NOT EQUAL ZEROS
               MOVE 'S'                TO WAFK-FIM
               MOVE GFCTGC-ERRO        TO WAFK-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAFK-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAFK-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO WAFK-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFK-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

       2312-OBTER-DESC-CBANC           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE WAFJ-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAFJ-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE WAFJ-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC OF GFCTB0L3     TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTGB-COD-DEPDC
           MOVE CPOSTO-SERVC OF GFCTB0L3
                                       TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTGB-COD-POSTO


           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 9                  TO WAFK-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0144'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR

           END-IF

           IF  GFCTGC-ERRO            NOT EQUAL ZEROS
               MOVE 'S'                TO WAFK-FIM
               MOVE GFCTGC-ERRO        TO WAFK-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAFK-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAFK-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO WAFK-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFK-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2312-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2320-OBTER-EMPRESA              SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L1.
           MOVE CSERVC-TARIF           OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L1.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L1.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       HINCL_REG,
                       CAG_BCRIA,
                       CCTA_BCRIA_CLI
               INTO    :GFCTB0L1.CAGPTO-CTA,
                       :GFCTB0L1.CSERVC-TARIF,
                       :GFCTB0L1.HINCL-REG,
                       :GFCTB0L1.CAG-BCRIA,
                       :GFCTB0L1.CCTA-BCRIA-CLI
               FROM    DB2PRD.TCANCT_ADSAO_EMPR
               WHERE   CAGPTO_CTA         = :GFCTB0L1.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0L1.CSERVC-TARIF
                 AND   HINCL_REG          = :GFCTB0L1.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_EMPR'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAFK-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L1'         TO WRK-NOME-TAB
               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL +100
               MOVE 1574               TO WAFK-COD-MSG-ERRO
               MOVE '1574'             TO GFCTG2-COD-MSG
           ELSE
               MOVE CAG-BCRIA OF GFCTB0L1 TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAFK-AGENCIA(IND-1)
               MOVE CCTA-BCRIA-CLI OF GFCTB0L1
                                       TO WRK-AUX-13-S
               MOVE WRK-AUX-13         TO WAFK-CONTA(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2330-OBTER-FORNEC               SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L2.
           MOVE CSERVC-TARIF           OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L2.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L2.

           EXEC SQL
ST2506*        SELECT  CCNPJ_CPF,
ST2506*                CFLIAL_CNPJ,
ST2506*                CCTRL_CNPJ
ST2506         SELECT  CCNPJ_CPF_ST,
ST2506                 CFLIAL_CNPJ_ST,
ST2506                 CCTRL_CNPJ_ST
ST2506*        INTO    :GFCTB0L2.CCNPJ-CPF,
ST2506*                :GFCTB0L2.CFLIAL-CNPJ,
ST2506*                :GFCTB0L2.CCTRL-CNPJ
ST2506        INTO    :GFCTB0L2.CCNPJ-CPF-ST,
ST2506                :GFCTB0L2.CFLIAL-CNPJ-ST,
ST2506                :GFCTB0L2.CCTRL-CNPJ-ST
               FROM    DB2PRD.TCANCT_ADSAO_FORNC
               WHERE   CAGPTO_CTA         = :GFCTB0L2.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0L2.CSERVC-TARIF
                 AND   HINCL_REG          = :GFCTB0L2.HINCL-REG
           END-EXEC.


           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_FORNC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAFK-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L2'         TO WRK-NOME-TAB
               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL +100
               MOVE 1575               TO WAFK-COD-MSG-ERRO
               MOVE '1575'             TO GFCTG2-COD-MSG

           ELSE
ST2506*        MOVE CCNPJ-CPF OF GFCTB0L2
ST2506*                                TO WRK-AUX-CNPJ
ST2506*        MOVE WRK-AUX-PJ         TO WAFK-CNPJ-NUM(IND-1)
ST2506*        MOVE CFLIAL-CNPJ OF GFCTB0L2
ST2506*                                TO WRK-AUX-5-S
ST2506*        MOVE WRK-AUX-5          TO WAFK-CNPJ-FIL(IND-1)
ST2506*        MOVE CCTRL-CNPJ OF GFCTB0L2
ST2506*                                TO WRK-AUX-2-S
ST2506*        MOVE WRK-AUX-2          TO WAFK-CNPJ-CTR(IND-1)

ST2506         MOVE CCNPJ-CPF-ST       OF GFCTB0L2         
ST2506                                 TO WAFK-CNPJ-CGC(IND-1)
ST2506         MOVE CFLIAL-CNPJ-ST     OF GFCTB0L2
ST2506                                 TO WAFK-CNPJ-FIL(IND-1)
ST2506         MOVE CCTRL-CNPJ-ST      OF GFCTB0L2
ST2506                                 TO WRK-AUX-2-S
ST2506         MOVE WRK-AUX-2          TO WAFK-CNPJ-CTR(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-TRATAR-CURSOR-2            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-ABRIR-CSR02-GFCTB0L0

           MOVE WAFJ-QTDE-OCOR         TO WRK-DESPREZA

           PERFORM 2420-LER-CSR02-GFCTB0L0

           PERFORM 2420-LER-CSR02-GFCTB0L0  VARYING  IND-1
                   FROM 1              BY 1
                   UNTIL IND-1         GREATER WRK-DESPREZA OR
                   WRK-FINALIZAR       EQUAL 'S'.

           MOVE ZEROS                  TO IND-1

           PERFORM 2430-PROCESSAR-CURSOR-2
                                       UNTIL WRK-FINALIZAR EQUAL 'S'

           PERFORM 2440-FECHAR-CSR02-GFCTB0L0.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-ABRIR-CSR02-GFCTB0L0       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR02-GFCTB0L0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAFK-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L0'         TO WRK-NOME-TAB

               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2420-LER-CSR02-GFCTB0L0         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR02-GFCTB0L0
               INTO    :GFCTB0L0.CAGPTO-CTA,
                       :GFCTB0L0.CSERVC-TARIF,
                       :GFCTB0L0.HINCL-REG,
                       :GFCTB0L0.CFUNC-SOLTC-CANCT,
                       :GFCTB0L0.CFUNC-MANUT,
                       :GFCTB0L0.CSIT-SOLTC-CANCT,
                       :GFCTB0L0.QPRVIO-CANCT-ADSAO,
                       :GFCTB0L0.QADSAO-CANCD,
                       :GFCTB0L0.HEXCUC-CALC-PRVIO,
                       :GFCTB0L0.HEXCUC-PROCM,
                       :GFCTB0L0.HMANUT-REG,
                       :GFCTB0L0.RJUSTF-SOLTC-CANCT,
                       :GFCTB0L0.RJUSTF-PAREC-CANCT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAFK-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L0'         TO WRK-NOME-TAB

               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          WAFK-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO WAFK-ERRO
                   MOVE ZEROS          TO WAFK-COD-SQL-ERRO
                   MOVE 0484           TO WAFK-COD-MSG-ERRO
                   MOVE '0484'         TO GFCTG2-COD-MSG
                   PERFORM 1100-OBTER-DESC-MSG
                   MOVE 'S'            TO WAFK-FIM
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2430-PROCESSAR-CURSOR-2         SECTION.
      *----------------------------------------------------------------*

           IF  CSERVC-TARIF OF GFCTB0L0
                                       NOT EQUAL WRK-TARIFA
               PERFORM 2231-ACESSAR-GFCTB0D8

               MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO WRK-TARIFA
           END-IF.

           ADD 1                       TO IND-1
           PERFORM 2300-MOVER-CAMPOS

           PERFORM 2420-LER-CSR02-GFCTB0L0

           IF  IND-1                   EQUAL 7
               IF  WRK-FINALIZAR       NOT EQUAL 'S'
                   MOVE 'S'            TO WRK-FINALIZAR

                   MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO WRK-AUX-5-S
                   MOVE WRK-AUX-5      TO WAFK-PNT-TARIFA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2440-FECHAR-CSR02-GFCTB0L0      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB0L0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAFK-COD-SQL-ERRO
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAFK-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L0'         TO WRK-NOME-TAB
               PERFORM 2211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*