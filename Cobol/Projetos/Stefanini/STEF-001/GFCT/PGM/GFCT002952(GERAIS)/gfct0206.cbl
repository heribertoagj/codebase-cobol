      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0206.
       AUTHOR. MIRELLI VENTURA
      *================================================================*
      *                   C P M  S I S T E M A S                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA       : GFCT0206                                   *
      *    PROGRAMADORA   : MIRELLI VENTURA             - CPM/FPOLIS   *
      *    ANALISTA CPM   : DIRCEU DE MATOS ALVES JR.   - CPM/FPOLIS   *
      *    ANALISTA       : MAURICIO - PROCWORK         - GRUPO 50     *
      *    DATA           : 12/04/2005                                 *
      *                                                                *
      *    OBJETIVO       :                                            *
      *      CADASTRO DE TARIFAS - INCLUSAO (PAGINA 4 DE 5).           *
      *                                                                *
      *    BANCO DE DADOS :                                            *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.HIST_EVNTO_CTBIL                 GFCTB085          *
      *      DB2PRD.HIST_TARIF_PRINC                 GFCTB087          *
      *      DB2PRD.OPERACAO_CTBIL                   TRICB003          *
      *      DB2PRD.DEPDC_BDSCO_ONLINE               MESUB010          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7203.                                    *
      *                                                                *
      *----------------------------------------------------------------*
      *               U L T I M A    A L T E R A C A O                 *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADORA........: ANA CAROLINA   CPM                    *
      *    ANALISTA............: MAURICIO       PWI  GP 50             *
      *    DATA................: 13/06/2005                            *
      *    OBJETIVO............: ALTERAR ROTINA TRATAR AVANCO          *
      *                                                                *
      *----------------------------------------------------------------*
      *               U L T I M A    A L T E R A C A O                 *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADORA........: ANA CAROLINA   CPM                    *
      *    ANALISTA............: MAURICIO       PWI  GP 50             *
      *    DATA................: 14/06/2005                            *
      *    OBJETIVO............: NAO PERMITIR A  INCLUSAO, QUANDO      *
      *                          SITUACAO DE LIBERACAO  FOR 6  OU      *
      *                          7. PERMITIR A INCLUSAO DE APENAS      *
      *                          5 TRILHAS CONTABEIS.                  *
      *                                                                *
SP1204*----------------------------------------------------------------*
SP1204*                 U L T I M A   A L T E R A C A O                *
SP1204*----------------------------------------------------------------*
SP1204*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - SP1204       *
SP1204*----------------------------------------------------------------*
SP1204*                                                                *
SP1204*    ANALISTA....:  MARCUS VINICIUS - SONDA/PROCWORK             *
SP1204*    DATA........:  12/04/2008                                   *
SP1204*    OBJETIVO....:  TRATAR COBRANCA E ESTORNO PARA TIPO DE       *
SP1204*                   PESSOA JURIDICA E AMBAS (FISICA E JURIDICA)  *
SP1204*                                                                *
BI0311*----------------------------------------------------------------*
BI0311*                 U L T I M A   A L T E R A C A O                *
BI0311*----------------------------------------------------------------*
BI0311*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0311       *
BI0311*----------------------------------------------------------------*
BI0311*                                                                *
BI0311*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0311*    DATA........:  03 / 2011                                    *
BI0311*    OBJETIVO....:  TRATAR O CAMPO NOVO CLCTO-ESTRN-ANTER DE     *
BI0311*                   ESTORNO DE TARIFAS SEMESTRE ANTERIOR.        *
BI0311*                                                                *
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
       77  FILLER                      PIC  X(32)          VALUE
            '*  INICIO DA WORKING GFCT0206  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
            '*       INDEXADORES            *'.
      *----------------------------------------------------------------*

       77  IND-OCOR                    PIC  9(03) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
            '*        AUXILIARES            *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(08)          VALUE SPACES.
       77  WRK-ERRO                    PIC  X(01)          VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(11)          VALUE SPACES.
       77  WRK-COUNT                   PIC S9(04)  COMP    VALUE ZEROS.
       77  WRK-QTDE-TOT-REG            PIC  9(02)          VALUE ZEROS.
       77  WRK-IND                     PIC  9(03)          VALUE ZEROS.
       77  WRK-PATRIC                  PIC  9(07)V9(04)    VALUE ZEROS.
       77  WRK-PERCENTUAL-TOTAL        PIC  9(07)V9(04)    VALUE ZEROS.

BI0311 01  WRK-S9-3                    PIC +9(009)        VALUE ZEROS.
BI0311 01  FILLER                      REDEFINES          WRK-S9-3.
BI0311     05  FILLER                  PIC  X(007).
BI0311     05  WRK-3-9                 PIC  9(003).

MBA   *----------------------------------------------------------------*
MBA    77  FILLER                      PIC  X(32)          VALUE
MBA         '*    AREA P/ MOD. GFCT5509    *'.
MBA   *----------------------------------------------------------------*

       COPY I#GFCTAN.
       COPY I#GFCTAO.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5000   *'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIADA.
               05  WRK-5000-AMBIENTE   PIC  X(01)        VALUE SPACES.
               05  WRK-5000-COD-MSG    PIC  9(04)        VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO
                                       PIC  9(02)        VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO
                                       PIC  9(03)        VALUE ZEROS.
               05  WRK-5000-DESC-MSG   PIC  X(70)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*   AREA P/ BANCO DE DADOS     *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB085
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB087
           END-EXEC.

           EXEC SQL
               INCLUDE TRICB003
           END-EXEC.

           EXEC SQL
               INCLUDE MESUB010
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*    FIM DA WORKING GFCT0206   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCT6X.

       COPY I#GFCT6Y.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCT6X-ENTRADA
                                                      GFCT6Y-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-MONTAR-AREA-SAIDA.

           PERFORM 200000-PROCESSAR-PROGRAMA.

           PERFORM 300000-RETORNAR-CHAMADOR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT6Y-SAIDA
                                          WRK-ERRO.

           INITIALIZE                  GFCT6Y-SAIDA.

           MOVE 1015                   TO GFCT6Y-LL.
           MOVE GFCT6X-ZZ              TO GFCT6Y-ZZ.
           MOVE GFCT6X-TRANSACAO       TO GFCT6Y-TRANSACAO.
           MOVE GFCT6X-FUNCAO          TO GFCT6Y-FUNCAO.
           MOVE ZEROS                  TO GFCT6Y-ERRO
                                          GFCT6Y-COD-SQL-ERRO
                                          GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 110000-CONSISTIR-AREA.

           PERFORM 120000-OBTER-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO GFCT6Y-DESC-MSG-ERRO.
           MOVE 'S'                    TO GFCT6Y-FIM.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-CONSISTIR-AREA           SECTION.
      *----------------------------------------------------------------*

           IF  GFCT6X-CSERVC-TARIF     IS NUMERIC
               MOVE GFCT6X-CSERVC-TARIF
                                       TO GFCT6Y-CSERVC-TARIF
           END-IF.

           IF  GFCT6X-AGEN-DEPTO       IS NUMERIC
               MOVE GFCT6X-AGEN-DEPTO  TO GFCT6Y-AGEN-DEPTO
           END-IF.

           IF  GFCT6X-CSEQ-MANUT-SERVC IS NUMERIC
               MOVE GFCT6X-CSEQ-MANUT-SERVC
                                       TO GFCT6Y-CSEQ-MANUT-SERVC
           END-IF.

           IF  GFCT6X-CTPO-LCTO-COBR   IS NUMERIC
               MOVE GFCT6X-CTPO-LCTO-COBR
                                       TO GFCT6Y-CTPO-LCTO-COBR
           END-IF.

           IF  GFCT6X-CTPO-LCTO-ESTOR  IS NUMERIC
               MOVE GFCT6X-CTPO-LCTO-ESTOR
                                       TO GFCT6Y-CTPO-LCTO-ESTOR
           END-IF.

SP1204     IF  GFCT6X-CTPO-LCTO-COBR-PJ
SP1204                                 IS NUMERIC
SP1204         MOVE GFCT6X-CTPO-LCTO-COBR-PJ
SP1204                                 TO GFCT6Y-CTPO-LCTO-COBR-PJ
SP1204     END-IF.
SP1204
SP1204     IF  GFCT6X-CTPO-LCTO-ESTOR-PJ
SP1204                                 IS NUMERIC
SP1204         MOVE GFCT6X-CTPO-LCTO-ESTOR-PJ
SP1204                                 TO GFCT6Y-CTPO-LCTO-ESTOR-PJ
SP1204     END-IF.
BI0311
BI0311     IF  GFCT6X-CLCTO-ESTRN-ANTER
BI0311                                 IS NUMERIC
BI0311         MOVE GFCT6X-CLCTO-ESTRN-ANTER
BI0311                                 TO GFCT6Y-CLCTO-ESTRN-ANTER
BI0311     END-IF.

           IF  GFCT6X-CTPO-SERVC-TARIF IS NUMERIC
               MOVE GFCT6X-CTPO-SERVC-TARIF
                                       TO GFCT6Y-CTPO-SERVC-TARIF
           END-IF.

           PERFORM VARYING IND-OCOR    FROM 1 BY 1
                   UNTIL   IND-OCOR    GREATER 5

               IF  GFCT6X-CTPO-LCTO-CTBIL(IND-OCOR)
                                       IS NUMERIC
                   MOVE GFCT6X-CTPO-LCTO-CTBIL(IND-OCOR)
                                       TO
                                       GFCT6Y-CTPO-LCTO-CTBIL(IND-OCOR)
               ELSE
                   MOVE 'S'            TO WRK-ERRO
               END-IF

               IF  GFCT6X-CSEQ-EVNTO-CTBIL(IND-OCOR)
                                       IS NUMERIC
                   MOVE GFCT6X-CSEQ-EVNTO-CTBIL(IND-OCOR)
                                       TO
                                       GFCT6Y-CSEQ-EVNTO-CTBIL(IND-OCOR)
               ELSE
                   MOVE 'S'            TO WRK-ERRO
               END-IF

               IF  GFCT6X-CINDCD-DEPDC-CTBIL(IND-OCOR)
                                       IS NUMERIC
                   MOVE GFCT6X-CINDCD-DEPDC-CTBIL(IND-OCOR)
                                       TO
                                     GFCT6Y-CINDCD-DEPDC-CTBIL(IND-OCOR)
               ELSE
                   MOVE 'S'            TO WRK-ERRO
               END-IF

               IF  GFCT6X-PATRIC-CTBIL(IND-OCOR)
                                       IS NUMERIC
                   MOVE GFCT6X-PATRIC-CTBIL(IND-OCOR)
                                       TO GFCT6Y-PATRIC-CTBIL(IND-OCOR)
               ELSE
                   MOVE 'S'            TO WRK-ERRO
               END-IF

               IF  GFCT6X-CJUNC-DEPDC-CTRLZ(IND-OCOR)
                                       IS NUMERIC
                   MOVE GFCT6X-CJUNC-DEPDC-CTRLZ(IND-OCOR)
                                       TO
                                     GFCT6Y-CJUNC-DEPDC-CTRLZ(IND-OCOR)
               ELSE
                   MOVE 'S'            TO WRK-ERRO
               END-IF

               MOVE GFCT6X-COPER-CTBIL(IND-OCOR)
                                       TO GFCT6Y-COPER-CTBIL(IND-OCOR)

           END-PERFORM.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-OBTER-MENSAGEM           SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIADA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCT6Y-ERRO
                   MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
                                          GFCT6Y-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT6Y-DESC-MSG-ERRO
               END-IF

               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCT6Y-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCT6Y-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCT6Y-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT6Y-DESC-MSG-ERRO
               END-IF

               MOVE SPACES             TO GFCT6Y-DADOS

               INITIALIZE              GFCT6Y-DADOS

               PERFORM 300000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           IF  GFCT6X-TRANSACAO        EQUAL SPACES OR
               GFCT6X-FUNCAO           EQUAL SPACES OR
               GFCT6X-ACAO             EQUAL SPACES OR
               GFCT6X-MASTER           EQUAL SPACES OR
               GFCT6X-FUNC-BDSCO       EQUAL SPACES OR
               GFCT6X-TERMINAL         EQUAL SPACES OR
               GFCT6X-DINIC-VGCIA-TARIF
                                       EQUAL SPACES OR
               GFCT6X-DFIM-VGCIA-TARIF EQUAL SPACES OR
               GFCT6X-HIDTFD-MANUT-SERVC
                                       EQUAL SPACES OR
               GFCT6X-RSERVC-TARIF-REDZD
                                       EQUAL SPACES OR
               GFCT6X-CSERVC-TARIF     NOT NUMERIC  OR
               GFCT6X-CTPO-SERVC-TARIF NOT NUMERIC  OR
               GFCT6X-CSEQ-MANUT-SERVC NOT NUMERIC  OR
               GFCT6X-AGEN-DEPTO       NOT NUMERIC  OR
               WRK-ERRO                EQUAL 'S'
               MOVE 1                  TO GFCT6Y-ERRO
               MOVE ZEROS              TO GFCT6Y-COD-SQL-ERRO
               MOVE 0001               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE WRK-5000-DESC-MSG  TO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR
           END-IF.

           PERFORM 210000-PROCESSAR-ACAO.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-PROCESSAR-ACAO           SECTION.
      *----------------------------------------------------------------*

MBA        IF  GFCT6X-ACAO             EQUAL 'A' OR 'C'
MBA            IF  GFCT6X-ACAO         EQUAL 'C'
MBA                PERFORM 212000-TRATAR-AVANCO
MBA
MBA                IF  GFCT6X-MASTER   EQUAL 'S'
MBA                    MOVE 7          TO CSIT-LIBRC-TARIF OF GFCTB087
MBA
MBA                    PERFORM 211000-ALTERAR-HISTORICO
MBA
MBA                    PERFORM 211100-ACESSAR-GFCT5509
MBA                ELSE
MBA                    MOVE 5          TO CSIT-LIBRC-TARIF OF GFCTB087
MBA
MBA                    PERFORM 211000-ALTERAR-HISTORICO
MBA                END-IF
MBA
MBA                PERFORM 300000-RETORNAR-CHAMADOR
MBA            END-IF

               PERFORM 212000-TRATAR-AVANCO

               MOVE 4                  TO CSIT-LIBRC-TARIF OF GFCTB087

               PERFORM 211000-ALTERAR-HISTORICO
           ELSE
               IF  GFCT6X-COPER-CTBIL(1)
                                       NOT EQUAL SPACES
                   MOVE 1              TO IND-OCOR

                   PERFORM 213000-INCLUIR-REGISTRO
               END-IF

               MOVE GFCT6X-CTPO-LCTO-COBR
                                       TO CTPO-LCTO-COBR  OF GFCTB087
               MOVE GFCT6X-CTPO-LCTO-ESTOR
                                       TO CTPO-LCTO-ESTRN OF GFCTB087

SP1204         MOVE GFCT6X-CTPO-LCTO-COBR-PJ
SP1204                                 TO CLCTO-COBR-PJ   OF GFCTB087
SP1204         MOVE GFCT6X-CTPO-LCTO-ESTOR-PJ
SP1204                                 TO CLCTO-ESTRN-PJ  OF GFCTB087

BI0311         MOVE GFCT6X-CLCTO-ESTRN-ANTER
BI0311                              TO CLCTO-ESTRN-ANTER  OF GFCTB087

               PERFORM 213400-ALTERAR-HISTORICO2
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-ALTERAR-HISTORICO        SECTION.
      *----------------------------------------------------------------*

           PERFORM  213410-SELECT-GFCTB087.

           MOVE GFCT6X-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB087.
           MOVE GFCT6X-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB087.
           MOVE GFCT6X-CSEQ-MANUT-SERVC
                                       TO CSEQ-MANUT-SERVC OF GFCTB087.

           EXEC SQL
                UPDATE DB2PRD.HIST_TARIF_PRINC
                SET   CSIT_LIBRC_TARIF   = :GFCTB087.CSIT-LIBRC-TARIF
                WHERE CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
                AND   HIDTFD_MANUT_SERVC = :GFCTB087.HIDTFD-MANUT-SERVC
                AND   CSEQ_MANUT_SERVC   = :GFCTB087.CSEQ-MANUT-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0019               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - GFCTB087'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO
           ELSE
               IF  SQLCODE             EQUAL +100
                   MOVE 1              TO GFCT6Y-ERRO
                   MOVE SQLCODE        TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0010           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB087'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

MBA   *----------------------------------------------------------------*
MBA    211100-ACESSAR-GFCT5509         SECTION.
MBA   *----------------------------------------------------------------*
MBA
MBA        MOVE  GFCT6X-TRANSACAO      TO  GFCTAN-TRANSACAO
MBA        MOVE  GFCT6X-FUNCAO         TO  GFCTAN-FUNCAO
MBA        MOVE  GFCT6X-CSERVC-TARIF   TO  GFCTAN-CSERVC-TARIF
MBA        MOVE  GFCT6X-HIDTFD-MANUT-SERVC
MBA                                    TO  GFCTAN-HIDTFD-MANUT-SERVC
MBA        MOVE  1                     TO  GFCTAN-IND-FUNCAO
MBA        MOVE  'S'                   TO  GFCTAN-FIM
MBA        MOVE 'GFCT5509'             TO WRK-MODULO.
MBA
MBA        CALL WRK-MODULO             USING GFCTAN-ENTRADA
MBA                                          GFCTAO-SAIDA
MBA                                          GFCT0M-AREA-ERROS.
MBA        IF  RETURN-CODE             NOT EQUAL ZEROS
MBA            MOVE 1                  TO GFCT6Y-ERRO
MBA            MOVE 0005               TO GFCT6Y-COD-MSG-ERRO
MBA                                       WRK-5000-COD-MSG
MBA
MBA            PERFORM 120000-OBTER-MENSAGEM
MBA
MBA            MOVE WRK-5000-DESC-MSG  TO GFCT6Y-DESC-MSG-ERRO
MBA
MBA            INITIALIZE GFCT6Y-DADOS
MBA
MBA            PERFORM 300000-RETORNAR-CHAMADOR
MBA        END-IF.
MBA
MBA        IF  GFCTAO-ERRO             EQUAL 9
MBA            MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
MBA            MOVE 'GFCT5509'         TO GFCT0M-PROGRAMA
MBA            MOVE 9                  TO GFCT6Y-ERRO
MBA            MOVE GFCTAO-DESC-MSG-ERRO
MBA                                    TO GFCT6Y-DESC-MSG-ERRO
MBA
MBA            INITIALIZE GFCT6Y-DADOS
MBA
MBA            PERFORM 300000-RETORNAR-CHAMADOR
MBA        END-IF.
MBA
MBA        IF  GFCTAO-ERRO             EQUAL 1
MBA            MOVE GFCTAO-DESC-MSG-ERRO
MBA                                    TO GFCT6Y-DESC-MSG-ERRO
MBA                                       GFCT0M-TEXTO
MBA            MOVE 1                  TO GFCT6Y-ERRO
MBA
MBA            INITIALIZE GFCT6Y-DADOS
MBA
MBA            PERFORM 300000-RETORNAR-CHAMADOR
MBA         END-IF.
MBA
MBA   *----------------------------------------------------------------*
MBA    211100-99-FIM.                  EXIT.
MBA   *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212000-TRATAR-AVANCO            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT6X-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB087.
           MOVE GFCT6X-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB087.
           MOVE GFCT6X-CSEQ-MANUT-SERVC
                                       TO CSEQ-MANUT-SERVC OF GFCTB087.

           EXEC SQL
                SELECT
                      CTPO_LCTO_COBR,
                      CTPO_LCTO_ESTRN,
SP1204                CLCTO_COBR_PJ,
SP1204                CLCTO_ESTRN_PJ,
BI0311                CLCTO_ESTRN_ANTER
                  INTO
                      :GFCTB087.CTPO-LCTO-COBR,
                      :GFCTB087.CTPO-LCTO-ESTRN,
SP1204                :GFCTB087.CLCTO-COBR-PJ,
SP1204                :GFCTB087.CLCTO-ESTRN-PJ,
BI0311                :GFCTB087.CLCTO-ESTRN-ANTER
                FROM   DB2PRD.HIST_TARIF_PRINC
                WHERE CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
                AND   HIDTFD_MANUT_SERVC = :GFCTB087.HIDTFD-MANUT-SERVC
                AND   CSEQ_MANUT_SERVC   = :GFCTB087.CSEQ-MANUT-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0019               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - GFCTB087'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO
           ELSE
               IF  SQLCODE             EQUAL +100
                   MOVE 1              TO GFCT6Y-ERRO
                   MOVE SQLCODE        TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0054           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB087'      TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
               END-IF
           END-IF.

SP1204     IF  GFCT6X-CTPO-LCTO-COBR   NOT EQUAL ZEROS
               MOVE GFCT6X-CSERVC-TARIF
                                       TO CSERVC-TARIF     OF GFCTB085
               MOVE GFCT6X-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB085
               MOVE GFCT6X-CSEQ-MANUT-SERVC
                                       TO CSEQ-MANUT-SERVC OF GFCTB085
               MOVE GFCT6X-CTPO-LCTO-COBR
                                       TO CTPO-LCTO-CTBIL  OF GFCTB085

               EXEC SQL
                 SELECT  CTPO_LCTO_CTBIL
                   INTO :GFCTB085.CTPO-LCTO-CTBIL
                   FROM  DB2PRD.HIST_EVNTO_CTBIL
                  WHERE  CSERVC_TARIF   = :GFCTB085.CSERVC-TARIF
                    AND  HIDTFD_MANUT_SERVC
                                        = :GFCTB085.HIDTFD-MANUT-SERVC
                    AND  CSEQ_MANUT_SERVC
                                        = :GFCTB085.CSEQ-MANUT-SERVC
                    AND  CTPO_LCTO_CTBIL
                                        = :GFCTB085.CTPO-LCTO-CTBIL
               END-EXEC

               IF (SQLCODE             NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
                  (SQLWARN0            EQUAL 'W')
                   MOVE SPACES         TO GFCT0M-AREA-ERROS

                   INITIALIZE             GFCT0M-AREA-ERROS

                   MOVE 'GFCT0206'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'HIST_EVNTO_CTBIL'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE SQLCODE        TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
                   MOVE '0030'         TO GFCT0M-LOCAL
                   MOVE SQLCA          TO GFCT0M-SQLCA-AREA
                   MOVE 9              TO GFCT6Y-ERRO
                   MOVE 0012           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB085'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               END-IF

               IF  SQLCODE             EQUAL +100
                   MOVE 01             TO GFCT6Y-ERRO
                   MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0164           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB085'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               END-IF
           END-IF.

SP1204     IF  GFCT6X-CTPO-LCTO-COBR-PJ
SP1204                                 NOT EQUAL ZEROS
SP1204         MOVE GFCT6X-CSERVC-TARIF
SP1204                                 TO CSERVC-TARIF     OF GFCTB085
SP1204         MOVE GFCT6X-HIDTFD-MANUT-SERVC
SP1204                                 TO HIDTFD-MANUT-SERVC
SP1204                                                     OF GFCTB085
SP1204         MOVE GFCT6X-CSEQ-MANUT-SERVC
SP1204                                 TO CSEQ-MANUT-SERVC OF GFCTB085
SP1204         MOVE GFCT6X-CTPO-LCTO-COBR-PJ
SP1204                                 TO CTPO-LCTO-CTBIL  OF GFCTB085
SP1204
SP1204         EXEC SQL
SP1204           SELECT  CTPO_LCTO_CTBIL
SP1204             INTO :GFCTB085.CTPO-LCTO-CTBIL
SP1204             FROM  DB2PRD.HIST_EVNTO_CTBIL
SP1204            WHERE  CSERVC_TARIF   = :GFCTB085.CSERVC-TARIF
SP1204              AND  HIDTFD_MANUT_SERVC
SP1204                                  = :GFCTB085.HIDTFD-MANUT-SERVC
SP1204              AND  CSEQ_MANUT_SERVC
SP1204                                  = :GFCTB085.CSEQ-MANUT-SERVC
SP1204              AND  CTPO_LCTO_CTBIL
SP1204                                  = :GFCTB085.CTPO-LCTO-CTBIL
SP1204         END-EXEC
SP1204
SP1204         IF (SQLCODE             NOT EQUAL ZEROS AND +100
SP1204                                                 AND -811) OR
SP1204            (SQLWARN0            EQUAL 'W')
SP1204             MOVE SPACES         TO GFCT0M-AREA-ERROS
SP1204
SP1204             INITIALIZE             GFCT0M-AREA-ERROS
SP1204
SP1204             MOVE 'GFCT0206'     TO GFCT0M-PROGRAMA
SP1204             MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
SP1204             MOVE 'HIST_EVNTO_CTBIL'
SP1204                                 TO GFCT0M-NOME-TAB
SP1204             MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
SP1204             MOVE SQLCODE        TO GFCT0M-SQLCODE
SP1204                                    GFCT6Y-COD-SQL-ERRO
SP1204             MOVE '0040'         TO GFCT0M-LOCAL
SP1204             MOVE SQLCA          TO GFCT0M-SQLCA-AREA
SP1204             MOVE 9              TO GFCT6Y-ERRO
SP1204             MOVE 0012           TO GFCT6Y-COD-MSG-ERRO
SP1204                                    WRK-5000-COD-MSG
SP1204
SP1204             PERFORM 120000-OBTER-MENSAGEM
SP1204
SP1204             MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
SP1204             MOVE ' - GFCTB085'  TO WRK-NOM-TAB
SP1204
SP1204             STRING WRK-5000-DESC-MSG
SP1204                    WRK-NOM-TAB
SP1204             DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
SP1204
SP1204             PERFORM 300000-RETORNAR-CHAMADOR
SP1204         END-IF
SP1204
SP1204         IF  SQLCODE             EQUAL +100
SP1204             MOVE 01             TO GFCT6Y-ERRO
SP1204             MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
SP1204             MOVE 1416           TO GFCT6Y-COD-MSG-ERRO
SP1204                                    WRK-5000-COD-MSG
SP1204
SP1204             PERFORM 120000-OBTER-MENSAGEM
SP1204
SP1204             MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
SP1204             MOVE ' - GFCTB085'  TO WRK-NOM-TAB
SP1204
SP1204             STRING WRK-5000-DESC-MSG
SP1204                    WRK-NOM-TAB
SP1204             DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
SP1204
SP1204             PERFORM 300000-RETORNAR-CHAMADOR
SP1204         END-IF
SP1204     END-IF.

BI0311     IF  GFCT6X-CLCTO-ESTRN-ANTER
BI0311                                 NOT EQUAL ZEROS
BI0311         MOVE GFCT6X-CSERVC-TARIF
BI0311                                 TO CSERVC-TARIF     OF GFCTB085
BI0311         MOVE GFCT6X-HIDTFD-MANUT-SERVC
BI0311                                 TO HIDTFD-MANUT-SERVC
BI0311                                                     OF GFCTB085
BI0311         MOVE GFCT6X-CSEQ-MANUT-SERVC
BI0311                                 TO CSEQ-MANUT-SERVC OF GFCTB085
BI0311         MOVE GFCT6X-CLCTO-ESTRN-ANTER
BI0311                                 TO CTPO-LCTO-CTBIL  OF GFCTB085
BI0311
BI0311         EXEC SQL
BI0311           SELECT  CTPO_LCTO_CTBIL
BI0311             INTO :GFCTB085.CTPO-LCTO-CTBIL
BI0311             FROM  DB2PRD.HIST_EVNTO_CTBIL
BI0311            WHERE  CSERVC_TARIF   = :GFCTB085.CSERVC-TARIF
BI0311              AND  HIDTFD_MANUT_SERVC
BI0311                                  = :GFCTB085.HIDTFD-MANUT-SERVC
BI0311              AND  CSEQ_MANUT_SERVC
BI0311                                  = :GFCTB085.CSEQ-MANUT-SERVC
BI0311              AND  CTPO_LCTO_CTBIL
BI0311                                  = :GFCTB085.CTPO-LCTO-CTBIL
BI0311         END-EXEC
BI0311
BI0311         IF (SQLCODE             NOT EQUAL ZEROS AND +100
BI0311                                                 AND -811) OR
BI0311            (SQLWARN0            EQUAL 'W')
BI0311             MOVE SPACES         TO GFCT0M-AREA-ERROS
BI0311
BI0311             INITIALIZE             GFCT0M-AREA-ERROS
BI0311
BI0311             MOVE 'GFCT0206'     TO GFCT0M-PROGRAMA
BI0311             MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
BI0311             MOVE 'HIST_EVNTO_CTBIL'
BI0311                                 TO GFCT0M-NOME-TAB
BI0311             MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
BI0311             MOVE SQLCODE        TO GFCT0M-SQLCODE
BI0311                                    WRK-S9-3
BI0311             MOVE WRK-3-9        TO GFCT6Y-COD-SQL-ERRO
BI0311             MOVE '0050'         TO GFCT0M-LOCAL
BI0311             MOVE SQLCA          TO GFCT0M-SQLCA-AREA
BI0311             MOVE 9              TO GFCT6Y-ERRO
BI0311             MOVE 0012           TO GFCT6Y-COD-MSG-ERRO
BI0311                                    WRK-5000-COD-MSG
BI0311
BI0311             PERFORM 120000-OBTER-MENSAGEM
BI0311
BI0311             MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
BI0311             MOVE ' - GFCTB085'  TO WRK-NOM-TAB
BI0311
BI0311             STRING WRK-5000-DESC-MSG
BI0311                    WRK-NOM-TAB
BI0311             DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
BI0311
BI0311             PERFORM 300000-RETORNAR-CHAMADOR
BI0311         END-IF
BI0311
BI0311         IF  SQLCODE             EQUAL +100
BI0311             MOVE 1              TO GFCT6Y-ERRO
BI0311             MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
BI0311             MOVE 1835           TO GFCT6Y-COD-MSG-ERRO
BI0311                                    WRK-5000-COD-MSG
BI0311
BI0311             PERFORM 120000-OBTER-MENSAGEM
BI0311
BI0311             MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
BI0311             MOVE ' - GFCTB085'  TO WRK-NOM-TAB
BI0311
BI0311             STRING WRK-5000-DESC-MSG
BI0311                    WRK-NOM-TAB
BI0311             DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
BI0311
BI0311             PERFORM 300000-RETORNAR-CHAMADOR
BI0311         END-IF
BI0311     END-IF.

           PERFORM 212100-TRATAR-AVANCO2.

      *----------------------------------------------------------------*
       212000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212100-TRATAR-AVANCO2           SECTION.
      *----------------------------------------------------------------*

SP1204     IF  GFCT6X-CTPO-LCTO-ESTOR  NOT EQUAL ZEROS
               MOVE GFCT6X-CSERVC-TARIF
                                       TO CSERVC-TARIF     OF GFCTB085
               MOVE GFCT6X-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB085
               MOVE GFCT6X-CSEQ-MANUT-SERVC
                                       TO CSEQ-MANUT-SERVC OF GFCTB085
               MOVE GFCT6X-CTPO-LCTO-ESTOR
                                       TO CTPO-LCTO-CTBIL  OF GFCTB085

               EXEC SQL
                 SELECT  CTPO_LCTO_CTBIL
                   INTO :GFCTB085.CTPO-LCTO-CTBIL
                   FROM  DB2PRD.HIST_EVNTO_CTBIL
                  WHERE  CSERVC_TARIF   = :GFCTB085.CSERVC-TARIF
                    AND  HIDTFD_MANUT_SERVC
                                        = :GFCTB085.HIDTFD-MANUT-SERVC
                    AND  CSEQ_MANUT_SERVC
                                        = :GFCTB085.CSEQ-MANUT-SERVC
                    AND  CTPO_LCTO_CTBIL
                                        = :GFCTB085.CTPO-LCTO-CTBIL
               END-EXEC

               IF (SQLCODE             NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
                  (SQLWARN0            EQUAL 'W')
                   MOVE SPACES         TO GFCT0M-AREA-ERROS

                   INITIALIZE             GFCT0M-AREA-ERROS

                   MOVE 'GFCT0206'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'HIST_EVNTO_CTBIL'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE SQLCODE        TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
                   MOVE '0060'         TO GFCT0M-LOCAL
                   MOVE SQLCA          TO GFCT0M-SQLCA-AREA
                   MOVE 9              TO GFCT6Y-ERRO
                   MOVE 0012           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB085'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               END-IF

               IF  SQLCODE             EQUAL +100
                   MOVE 01             TO GFCT6Y-ERRO
                   MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0165           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB085'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               END-IF
           END-IF.

SP1204     IF  GFCT6X-CTPO-LCTO-COBR-PJ
SP1204                                 NOT EQUAL ZEROS
SP1204         MOVE GFCT6X-CSERVC-TARIF
SP1204                                 TO CSERVC-TARIF     OF GFCTB085
SP1204         MOVE GFCT6X-HIDTFD-MANUT-SERVC
SP1204                                 TO HIDTFD-MANUT-SERVC
SP1204                                                     OF GFCTB085
SP1204         MOVE GFCT6X-CSEQ-MANUT-SERVC
SP1204                                 TO CSEQ-MANUT-SERVC OF GFCTB085
SP1204         MOVE GFCT6X-CTPO-LCTO-ESTOR-PJ
SP1204                                 TO CTPO-LCTO-CTBIL  OF GFCTB085
SP1204
SP1204         EXEC SQL
SP1204           SELECT  CTPO_LCTO_CTBIL
SP1204             INTO :GFCTB085.CTPO-LCTO-CTBIL
SP1204             FROM  DB2PRD.HIST_EVNTO_CTBIL
SP1204            WHERE  CSERVC_TARIF   = :GFCTB085.CSERVC-TARIF
SP1204              AND  HIDTFD_MANUT_SERVC
SP1204                                  = :GFCTB085.HIDTFD-MANUT-SERVC
SP1204              AND  CSEQ_MANUT_SERVC
SP1204                                  = :GFCTB085.CSEQ-MANUT-SERVC
SP1204              AND  CTPO_LCTO_CTBIL
SP1204                                  = :GFCTB085.CTPO-LCTO-CTBIL
SP1204         END-EXEC
SP1204
SP1204         IF (SQLCODE             NOT EQUAL ZEROS AND +100
SP1204                                                 AND -811) OR
SP1204            (SQLWARN0            EQUAL 'W')
SP1204             MOVE SPACES         TO GFCT0M-AREA-ERROS
SP1204
SP1204             INITIALIZE             GFCT0M-AREA-ERROS
SP1204
SP1204             MOVE 'GFCT0206'     TO GFCT0M-PROGRAMA
SP1204             MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
SP1204             MOVE 'HIST_EVNTO_CTBIL'
SP1204                                 TO GFCT0M-NOME-TAB
SP1204             MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
SP1204             MOVE SQLCODE        TO GFCT0M-SQLCODE
SP1204                                    GFCT6Y-COD-SQL-ERRO
SP1204             MOVE '0070'         TO GFCT0M-LOCAL
SP1204             MOVE SQLCA          TO GFCT0M-SQLCA-AREA
SP1204             MOVE 9              TO GFCT6Y-ERRO
SP1204             MOVE 0012           TO GFCT6Y-COD-MSG-ERRO
SP1204                                    WRK-5000-COD-MSG
SP1204
SP1204             PERFORM 120000-OBTER-MENSAGEM
SP1204
SP1204             MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
SP1204             MOVE ' - GFCTB085'  TO WRK-NOM-TAB
SP1204
SP1204             STRING WRK-5000-DESC-MSG
SP1204                    WRK-NOM-TAB
SP1204             DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
SP1204
SP1204             PERFORM 300000-RETORNAR-CHAMADOR
SP1204         END-IF
SP1204
SP1204         IF  SQLCODE             EQUAL +100
SP1204             MOVE 01             TO GFCT6Y-ERRO
SP1204             MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
SP1204             MOVE 1417           TO GFCT6Y-COD-MSG-ERRO
SP1204                                    WRK-5000-COD-MSG
SP1204
SP1204             PERFORM 120000-OBTER-MENSAGEM
SP1204
SP1204             MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
SP1204             MOVE ' - GFCTB085'  TO WRK-NOM-TAB
SP1204
SP1204             STRING WRK-5000-DESC-MSG
SP1204                    WRK-NOM-TAB
SP1204             DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
SP1204
SP1204             PERFORM 300000-RETORNAR-CHAMADOR
SP1204         END-IF
SP1204     END-IF.

BI0311     IF  GFCT6X-CLCTO-ESTRN-ANTER
BI0311                                 NOT EQUAL ZEROS
BI0311         MOVE GFCT6X-CSERVC-TARIF
BI0311                                 TO CSERVC-TARIF     OF GFCTB085
BI0311         MOVE GFCT6X-HIDTFD-MANUT-SERVC
BI0311                                 TO HIDTFD-MANUT-SERVC
BI0311                                                     OF GFCTB085
BI0311         MOVE GFCT6X-CSEQ-MANUT-SERVC
BI0311                                 TO CSEQ-MANUT-SERVC OF GFCTB085
BI0311         MOVE GFCT6X-CLCTO-ESTRN-ANTER
BI0311                                 TO CTPO-LCTO-CTBIL  OF GFCTB085
BI0311
BI0311         EXEC SQL
BI0311           SELECT  CTPO_LCTO_CTBIL
BI0311             INTO :GFCTB085.CTPO-LCTO-CTBIL
BI0311             FROM  DB2PRD.HIST_EVNTO_CTBIL
BI0311            WHERE  CSERVC_TARIF   = :GFCTB085.CSERVC-TARIF
BI0311              AND  HIDTFD_MANUT_SERVC
BI0311                                  = :GFCTB085.HIDTFD-MANUT-SERVC
BI0311              AND  CSEQ_MANUT_SERVC
BI0311                                  = :GFCTB085.CSEQ-MANUT-SERVC
BI0311              AND  CTPO_LCTO_CTBIL
BI0311                                  = :GFCTB085.CTPO-LCTO-CTBIL
BI0311         END-EXEC
BI0311
BI0311         IF (SQLCODE             NOT EQUAL ZEROS AND +100
BI0311                                                 AND -811) OR
BI0311            (SQLWARN0            EQUAL 'W')
BI0311             MOVE SPACES         TO GFCT0M-AREA-ERROS
BI0311
BI0311             INITIALIZE             GFCT0M-AREA-ERROS
BI0311
BI0311             MOVE 'GFCT0206'     TO GFCT0M-PROGRAMA
BI0311             MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
BI0311             MOVE 'HIST_EVNTO_CTBIL'
BI0311                                 TO GFCT0M-NOME-TAB
BI0311             MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
BI0311             MOVE SQLCODE        TO GFCT0M-SQLCODE
BI0311                                    WRK-S9-3
BI0311             MOVE WRK-3-9        TO GFCT6Y-COD-SQL-ERRO
BI0311             MOVE '0080'         TO GFCT0M-LOCAL
BI0311             MOVE SQLCA          TO GFCT0M-SQLCA-AREA
BI0311             MOVE 9              TO GFCT6Y-ERRO
BI0311             MOVE 0012           TO GFCT6Y-COD-MSG-ERRO
BI0311                                    WRK-5000-COD-MSG
BI0311
BI0311             PERFORM 120000-OBTER-MENSAGEM
BI0311
BI0311             MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
BI0311             MOVE ' - GFCTB085'  TO WRK-NOM-TAB
BI0311
BI0311             STRING WRK-5000-DESC-MSG
BI0311                    WRK-NOM-TAB
BI0311             DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
BI0311
BI0311             PERFORM 300000-RETORNAR-CHAMADOR
BI0311         END-IF
BI0311
BI0311         IF  SQLCODE             EQUAL +100
BI0311             MOVE 1              TO GFCT6Y-ERRO
BI0311             MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
BI0311             MOVE 1835           TO GFCT6Y-COD-MSG-ERRO
BI0311                                    WRK-5000-COD-MSG
BI0311
BI0311             PERFORM 120000-OBTER-MENSAGEM
BI0311
BI0311             MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
BI0311             MOVE ' - GFCTB085'  TO WRK-NOM-TAB
BI0311
BI0311             STRING WRK-5000-DESC-MSG
BI0311                    WRK-NOM-TAB
BI0311             DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
BI0311
BI0311             PERFORM 300000-RETORNAR-CHAMADOR
BI0311         END-IF
BI0311     END-IF.

      *----------------------------------------------------------------*
       212100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213000-INCLUIR-REGISTRO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCT6X-CINDCD-DEPDC-CTBIL(IND-OCOR)
                                       EQUAL 3
               IF  GFCT6X-CJUNC-DEPDC-CTRLZ(IND-OCOR)
                                       EQUAL ZEROS
                   MOVE 1              TO GFCT6Y-ERRO
                   MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0002           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               ELSE
                   PERFORM 213100-VALIDAR-DEPENDENCIA
               END-IF
           END-IF.

           PERFORM 213200-VALIDAR-OPERACAO-TRIC.

           MOVE GFCT6X-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB085.
           MOVE GFCT6X-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB085.
           MOVE GFCT6X-CSEQ-MANUT-SERVC
                                       TO CSEQ-MANUT-SERVC OF GFCTB085.
           MOVE GFCT6X-CTPO-LCTO-CTBIL(IND-OCOR)
                                       TO CTPO-LCTO-CTBIL  OF GFCTB085.

           EXEC  SQL
             SELECT    MAX(CSEQ_EVNTO_CTBIL),
                       SUM(PATRIC_CTBIL)
               INTO    :GFCTB085.CSEQ-EVNTO-CTBIL,
                       :GFCTB085.PATRIC-CTBIL
               FROM    DB2PRD.HIST_EVNTO_CTBIL
              WHERE    CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
                AND    HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC
                AND    CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
                AND    CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
           END-EXEC.

           IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0012               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - MESUB010'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR.

           IF  SQLCODE  EQUAL -305
               MOVE ZEROS                TO GFCT6Y-CSEQ-MANUT-SERVC
               MOVE ZEROS                TO PATRIC-CTBIL OF GFCTB085
           ELSE
               MOVE CSEQ-EVNTO-CTBIL OF GFCTB085
                                         TO GFCT6Y-CSEQ-MANUT-SERVC.

           IF  SQLCODE           EQUAL ZEROS
               MOVE 1              TO GFCT6Y-ERRO
               MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
               MOVE 0229           TO GFCT6Y-COD-MSG-ERRO
                                      WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - GFCTB085'  TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR.

           MOVE  ZEROS    TO  WRK-QTDE-TOT-REG.
           MOVE  ZEROS    TO  WRK-PATRIC WRK-PERCENTUAL-TOTAL.

           PERFORM VARYING WRK-IND FROM 1 BY 1
             UNTIL GFCT6X-COPER-CTBIL(WRK-IND) EQUAL SPACES
                OR WRK-IND GREATER 5
                  ADD  1  TO WRK-QTDE-TOT-REG
                  ADD  GFCT6X-PATRIC-CTBIL(WRK-IND)
                                       TO WRK-PATRIC
           END-PERFORM

           IF  WRK-PATRIC    NOT EQUAL 100
               MOVE 1              TO GFCT6Y-ERRO
               MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
               MOVE 0215           TO GFCT6Y-COD-MSG-ERRO
                                      WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

MBA            MOVE WRK-5000-DESC-MSG TO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR.

           MOVE GFCT6X-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB085.
           MOVE GFCT6X-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB085.
           MOVE GFCT6X-CSEQ-MANUT-SERVC
                                       TO CSEQ-MANUT-SERVC OF GFCTB085.
           MOVE GFCT6X-CTPO-LCTO-CTBIL(IND-OCOR)
                                       TO CTPO-LCTO-CTBIL  OF GFCTB085.

           MOVE  ZEROS                 TO WRK-COUNT.

           EXEC  SQL
             SELECT  COUNT(*)
               INTO  :WRK-COUNT
               FROM  DB2PRD.HIST_EVNTO_CTBIL
                WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
                AND    HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC
                AND    CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
                AND    CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0012               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - MESUB010'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR
           ELSE
               IF  WRK-COUNT    EQUAL 5
                   MOVE 1              TO GFCT6Y-ERRO
                   MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0229           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB085'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               ELSE
                   IF  (WRK-QTDE-TOT-REG  +  WRK-COUNT)  >  5
                       MOVE 1              TO GFCT6Y-ERRO
                       MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
                       MOVE 0356           TO GFCT6Y-COD-MSG-ERRO
                                              WRK-5000-COD-MSG

                       PERFORM 120000-OBTER-MENSAGEM

                       MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                       MOVE ' - GFCTB085'  TO WRK-NOM-TAB

                       STRING WRK-5000-DESC-MSG
                              WRK-NOM-TAB
                       DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                       PERFORM 300000-RETORNAR-CHAMADOR
                   END-IF
               END-IF
           END-IF.

           PERFORM 213300-MONTAR-DADOS-INCLUSAO.

      *----------------------------------------------------------------*
       213000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213100-VALIDAR-DEPENDENCIA      SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CEMPR-INC OF MESUB010.
           MOVE GFCT6X-CJUNC-DEPDC-CTRLZ(IND-OCOR)
                                       TO CDEPDC    OF MESUB010.

           EXEC SQL
                SELECT CEMPR_INC
                INTO  :MESUB010.CEMPR-INC
                FROM   DB2PRD.DEPDC_BDSCO_ONLINE
                WHERE  CEMPR_INC       = :MESUB010.CEMPR-INC
                AND    CDEPDC          = :MESUB010.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'DEPDC_BDSCO_ONLINE'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0012               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - MESUB010'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR
           ELSE
               IF  SQLCODE             EQUAL +100
                   MOVE 01             TO GFCT6Y-ERRO
                   MOVE 1              TO
                                       GFCT6Y-COD-MSG-ERRO-GEN(IND-OCOR)
                   MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0112           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - MESUB010'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       213100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213200-VALIDAR-OPERACAO-TRIC    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CPRODT-CTBIL-ELEMT
                                                         OF TRICB003.
           MOVE GFCT6X-COPER-CTBIL(IND-OCOR)
                                       TO COPER-CTBIL    OF TRICB003.

           EXEC SQL
                SELECT CPRODT_CTBIL_ELEMT,
                       COPER_CTBIL
                INTO  :TRICB003.CPRODT-CTBIL-ELEMT,
                      :TRICB003.COPER-CTBIL
                FROM   DB2PRD.OPERACAO_CTBIL
                WHERE  CPRODT_CTBIL_ELEMT = :TRICB003.CPRODT-CTBIL-ELEMT
                AND    COPER_CTBIL        = :TRICB003.COPER-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'OPERACAO_CTBIL'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0012               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - TRICB003'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR
           ELSE
               IF  SQLCODE             EQUAL +100
                   MOVE 01             TO GFCT6Y-ERRO
                   MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0117           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - TRICB003'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       213200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213300-MONTAR-DADOS-INCLUSAO    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT6X-CSERVC-TARIF    TO CSERVC-TARIF       OF GFCTB085
                                          GFCT6Y-CSERVC-TARIF.
           MOVE GFCT6X-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC OF GFCTB085
                                          GFCT6Y-HIDTFD-MANUT-SERVC.
           MOVE GFCT6X-CPRODT-CTBIL-ELEMT
                                       TO GFCT6Y-CPRODT-CTBIL-ELEMT.
           MOVE ZEROS                  TO CPRODT-CTBIL      OF GFCTB085.
           MOVE GFCT6X-DINIC-VGCIA-TARIF
                                       TO GFCT6Y-DINIC-VGCIA-TARIF.
           MOVE GFCT6X-DFIM-VGCIA-TARIF
                                       TO GFCT6Y-DFIM-VGCIA-TARIF.
           MOVE GFCT6X-RSERVC-TARIF-REDZD
                                       TO GFCT6Y-RSERVC-TARIF-REDZD.
           MOVE GFCT6X-CTPO-SERVC-TARIF
                                       TO GFCT6Y-CTPO-SERVC-TARIF.
           MOVE GFCT6X-FUNC-BDSCO      TO GFCT6Y-FUNC-BDSCO.
           MOVE GFCT6X-TERMINAL        TO GFCT6Y-TERMINAL.
           MOVE GFCT6X-AGEN-DEPTO      TO GFCT6Y-AGEN-DEPTO.
           MOVE GFCT6X-ACAO            TO GFCT6Y-ACAO.

           PERFORM VARYING IND-OCOR    FROM 1 BY 1
                   UNTIL   IND-OCOR    GREATER 5 OR
                   GFCT6X-CTPO-LCTO-CTBIL(IND-OCOR)
                                       EQUAL ZEROS
MBA            IF GFCT6X-CINDCD-DEPDC-CTBIL(IND-OCOR)
MBA                                       EQUAL 3
MBA               IF  GFCT6X-CJUNC-DEPDC-CTRLZ(IND-OCOR)
MBA                                       EQUAL ZEROS
MBA                   MOVE 1              TO GFCT6Y-ERRO
MBA                   MOVE ZEROS          TO GFCT6Y-COD-SQL-ERRO
MBA                   MOVE 2              TO GFCT6Y-COD-MSG-ERRO
MBA                                          WRK-5000-COD-MSG
MBA
MBA                   PERFORM 120000-OBTER-MENSAGEM
MBA
MBA                   MOVE WRK-5000-DESC-MSG
MBA                                       TO GFCT6Y-DESC-MSG-ERRO
MBA
MBA                   INITIALIZE GFCT6Y-DADOS
MBA
MBA                   PERFORM 300000-RETORNAR-CHAMADOR
MBA               ELSE
MBA                   PERFORM 213100-VALIDAR-DEPENDENCIA
MBA               END-IF
MBA            END-IF

               MOVE GFCT6X-COPER-CTBIL(IND-OCOR)
                                       TO COPER-CTBIL        OF GFCTB085
               MOVE GFCT6X-CTPO-LCTO-CTBIL(IND-OCOR)
                                       TO CTPO-LCTO-CTBIL    OF GFCTB085
               ADD  1                  TO GFCT6Y-CSEQ-MANUT-SERVC
               MOVE GFCT6Y-CSEQ-MANUT-SERVC
                                       TO CSEQ-EVNTO-CTBIL   OF GFCTB085
               MOVE GFCT6X-CINDCD-DEPDC-CTBIL(IND-OCOR)
                                       TO CINDCD-DEPDC-CTBIL OF GFCTB085
               MOVE GFCT6X-PATRIC-CTBIL(IND-OCOR)
                                       TO PATRIC-CTBIL       OF GFCTB085
               MOVE GFCT6X-CJUNC-DEPDC-CTRLZ(IND-OCOR)
                                       TO CDEPDC-CTLZO       OF GFCTB085
MBA            MOVE ZEROS              TO CEMPR
MBA                                       CGRP-RZ-CTBIL      OF GFCTB085
MBA                                       CSGRP-RZ-CTBIL     OF GFCTB085
MBA                                       CCTA-EVNTO-CTBIL   OF GFCTB085
MBA                                       CDOCTO-CTBIL       OF GFCTB085

               PERFORM 213310-INSERIR-DADOS-GFCTB085

           END-PERFORM.

      *----------------------------------------------------------------*
       213300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213310-INSERIR-DADOS-GFCTB085   SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.HIST_EVNTO_CTBIL
                       (CSERVC_TARIF,
                        HIDTFD_MANUT_SERVC,
                        CSEQ_MANUT_SERVC,
                        CTPO_LCTO_CTBIL,
                        CSEQ_EVNTO_CTBIL,
                        CINDCD_DEPDC_CTBIL,
                        CPRODT_CTBIL,
                        COPER_CTBIL,
                        CDEPDC_CTLZO,
                        PATRIC_CTBIL,
                        CEMPR,
                        CGRP_RZ_CTBIL,
                        CSGRP_RZ_CTBIL ,
                        CCTA_EVNTO_CTBIL,
                        CDOCTO_CTBIL)
                VALUES (:GFCTB085.CSERVC-TARIF,
                        :GFCTB085.HIDTFD-MANUT-SERVC,
                        :GFCTB085.CSEQ-MANUT-SERVC,
                        :GFCTB085.CTPO-LCTO-CTBIL,
                        :GFCTB085.CSEQ-EVNTO-CTBIL,
                        :GFCTB085.CINDCD-DEPDC-CTBIL,
                        :GFCTB085.CPRODT-CTBIL,
                        :GFCTB085.COPER-CTBIL,
                        :GFCTB085.CDEPDC-CTLZO,
                        :GFCTB085.PATRIC-CTBIL,
                        :GFCTB085.CEMPR,
                        :GFCTB085.CGRP-RZ-CTBIL,
                        :GFCTB085.CSGRP-RZ-CTBIL,
                        :GFCTB085.CCTA-EVNTO-CTBIL,
                        :GFCTB085.CDOCTO-CTBIL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0018               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - GFCTB085'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       213310-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213400-ALTERAR-HISTORICO2       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT6X-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB087.
           MOVE GFCT6X-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB087.
           MOVE GFCT6X-CSEQ-MANUT-SERVC
                                       TO CSEQ-MANUT-SERVC OF GFCTB087.

           EXEC SQL
                UPDATE DB2PRD.HIST_TARIF_PRINC
                SET   CTPO_LCTO_COBR     = :GFCTB087.CTPO-LCTO-COBR,
                      CTPO_LCTO_ESTRN    = :GFCTB087.CTPO-LCTO-ESTRN,
SP1204                CLCTO_COBR_PJ      = :GFCTB087.CLCTO-COBR-PJ,
SP1204                CLCTO_ESTRN_PJ     = :GFCTB087.CLCTO-ESTRN-PJ,
BI0311                CLCTO_ESTRN_ANTER  = :GFCTB087.CLCTO-ESTRN-ANTER
                WHERE CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
                AND   HIDTFD_MANUT_SERVC = :GFCTB087.HIDTFD-MANUT-SERVC
                AND   CSEQ_MANUT_SERVC   = :GFCTB087.CSEQ-MANUT-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0019               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - GFCTB087'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO

               PERFORM 300000-RETORNAR-CHAMADOR
           ELSE
               IF  SQLCODE             EQUAL +100
                   MOVE 1              TO GFCT6Y-ERRO
                   MOVE SQLCODE        TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0010           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB087'  TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO

                   PERFORM 300000-RETORNAR-CHAMADOR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       213400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213410-SELECT-GFCTB087          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT6X-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB087.

           EXEC SQL
                SELECT
                      CTPO_LCTO_COBR,
                      CTPO_LCTO_ESTRN,
SP1204                CLCTO_COBR_PJ,
SP1204                CLCTO_ESTRN_PJ,
BI0311                CLCTO_ESTRN_ANTER
                  INTO
                      :GFCTB087.CTPO-LCTO-COBR,
                      :GFCTB087.CTPO-LCTO-ESTRN,
SP1204                :GFCTB087.CLCTO-COBR-PJ,
SP1204                :GFCTB087.CLCTO-ESTRN-PJ,
BI0311                :GFCTB087.CLCTO-ESTRN-ANTER
                FROM   DB2PRD.HIST_TARIF_PRINC
                WHERE CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
                AND ( CSIT_LIBRC_TARIF   = 6
                 OR   CSIT_LIBRC_TARIF   = 7  )
           END-EXEC.

           IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'GFCT0206'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCT6Y-COD-SQL-ERRO
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT6Y-ERRO
               MOVE 0019               TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 120000-OBTER-MENSAGEM

               MOVE SPACES             TO GFCT6Y-DESC-MSG-ERRO
               MOVE ' - GFCTB087'      TO WRK-NOM-TAB

               STRING WRK-5000-DESC-MSG
                      WRK-NOM-TAB
               DELIMITED BY '  '       INTO GFCT6Y-DESC-MSG-ERRO
           ELSE
               IF  SQLCODE             EQUAL ZEROS OR -811
                   MOVE 1              TO GFCT6Y-ERRO
                   MOVE SQLCODE        TO GFCT6Y-COD-SQL-ERRO
                   MOVE 0145           TO GFCT6Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 120000-OBTER-MENSAGEM

                   MOVE SPACES         TO GFCT6Y-DESC-MSG-ERRO
                   MOVE ' - GFCTB087'      TO WRK-NOM-TAB

                   STRING WRK-5000-DESC-MSG
                          WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCT6Y-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       213410-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-RETORNAR-CHAMADOR        SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
