      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT0146.
       AUTHOR.     SONDAPROCWORK OUTSOURCING.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0146                                    *
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK - GP.50  *
      *    DATA........:   26/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   LISTA AS SOLICITACOES DE EXCLUSAO MASSIVA   *
      *                    DE ADESAO PARA CONSULTA DETALHADA.          *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *           DB2PRD.TCANCT_ADSAO_AGPTO          GFCTB0L0          *
      *           DB2PRD.TCANCT_ADSAO_EMPR           GFCTB0L1          *
      *           DB2PRD.TCANCT_ADSAO_FORNC          GFCTB0L2          *
      *           DB2PRD.TCANCT_ADSAO_POSTO          GFCTB0L3          *
      *           DB2PRD.SERVC_TARIF_PRINC           GFCTB0D8          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *      GFCT5109 - OBTER DESCRICAO DO PAB                         *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
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
       01  FILLER                      PIC  X(032)         VALUE
           '*  INICIO DA WORKING GFCT0146  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-COD-TABELA              PIC  X(008)         VALUE SPACES.

       01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE.
           05  FILLER                  PIC  9(006).
           05  WRK-COD-SQL             PIC  9(003).

       01  WRK-QTDE-OCOR               PIC  9(003) COMP-3  VALUE ZEROS.

       01  WRK-WAFO-QTDE-OCOR          PIC  9(003) COMP-3  VALUE ZEROS.
       01  WRK-WAFO-QTDE-TOT-REG       PIC  9(006) COMP-3  VALUE ZEROS.

       01  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.

       01  WRK-CSERVC-TARIF            PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CSERVC-TARIF.
           05  WRK-COD-TARIFA          PIC  9(005).

       01  WRK-AGENCIA                 PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AGENCIA.
           05  WRK-COD-AGENCIA         PIC  9(005).

       01  WRK-POSTO                   PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-POSTO.
           05  WRK-COD-POSTO           PIC  9(005).

       01  WRK-CONTA                   PIC S9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CONTA.
           05  WRK-COD-CONTA           PIC  9(013).

ST2506*01  WRK-CCNPJ-CPF               PIC S9(009)         VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES WRK-CCNPJ-CPF.
ST2506*    05  WRK-COD-CNPJ            PIC  9(009).

ST2506*01  WRK-CFLIAL-CNPJ             PIC S9(005)         VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES WRK-CFLIAL-CNPJ.
ST2506*    05  WRK-COD-FILIAL          PIC  9(005).

       01  WRK-CCTRL-CNPJ              PIC S9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CCTRL-CNPJ.
           05  WRK-COD-CONTROLE        PIC  9(002).

       01  WRK-HINCL.
           05  WRK-ANO-INCL            PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-INCL            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-DIA-INCL            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(016)         VALUE SPACES.

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MSG001.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           05  WRK-MSG001-MODULO       PIC  X(008)         VALUE SPACES.

       01  WRK-MSG002.
           05  FILLER                  PIC  X(026)         VALUE
               'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.
       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.
       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* AREA DO MODULO GFCT5109/5090 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
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

           EXEC SQL
            DECLARE
                     CSR01-GFCTB0L0 CURSOR WITH HOLD FOR
             SELECT
                     CAGPTO_CTA      ,
                     CSERVC_TARIF    ,
                     HINCL_REG       ,
                     CSIT_SOLTC_CANCT
               FROM
                     DB2PRD.TCANCT_ADSAO_AGPTO
              WHERE
                     CAGPTO_CTA         = :GFCTB0L0.CAGPTO-CTA
           ORDER BY
                     CSERVC_TARIF,
                     HINCL_REG
           END-EXEC.

           EXEC SQL
            DECLARE
                     CSR02-GFCTB0L0 CURSOR WITH HOLD FOR
             SELECT
                     CAGPTO_CTA      ,
                     CSERVC_TARIF    ,
                     HINCL_REG       ,
                     CSIT_SOLTC_CANCT
               FROM
                     DB2PRD.TCANCT_ADSAO_AGPTO
              WHERE
                     CAGPTO_CTA         = :GFCTB0L0.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L0.CSERVC-TARIF
           ORDER BY
                     HINCL_REG
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    FIM DA WORKING GFCT0146   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAFN.
       COPY GFCTWAFO.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING WAFN-ENTRADA
                                             WAFO-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-ONLINE.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAFO-SAIDA.

           INITIALIZE                  WAFO-SAIDA.

           MOVE +149                   TO WAFO-LL.
           MOVE WAFN-TRANSACAO         TO WAFO-TRANSACAO.
           MOVE WAFN-FUNCAO            TO WAFO-FUNCAO.

           MOVE WAFN-QTDE-OCOR         TO WAFO-QTDE-OCOR.
           MOVE ZEROS                  TO WAFO-QTDE-TOT-REG.

           MOVE WAFN-FUNC-BDSCO        TO WAFO-FUNC-BDSCO.
           MOVE ZEROS                  TO WAFO-ERRO
                                          WAFO-COD-SQL-ERRO
                                          WAFO-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 9990-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WAFO-DESC-MSG-ERRO.

           MOVE WAFN-AGPTO             TO WAFO-AGPTO.
           MOVE WAFN-COD-PACOTE        TO WAFO-COD-PACOTE.
           MOVE 'N'                    TO WAFO-FIM.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO GFCTG1-SAIDA
                                          GFCTFZ-ENTRADA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTG1-SAIDA
                                       GFCTFZ-ENTRADA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE WAFN-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAFN-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAFN-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA

                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFO-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0146'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 9                  TO WAFO-ERRO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG1-COD-MSG-ERRO
                                       TO WAFO-COD-MSG-ERRO
                   MOVE GFCTG1-COD-SQL-ERRO

                                       TO WAFO-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAFO-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0146'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAFO-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0146'         TO GFCT0M-TRANSACAO
               MOVE WRK-MSG002         TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA

               PERFORM 9999-FINALIZAR

           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAFN-TRANSACAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAFN-FUNCAO             EQUAL SPACES OR LOW-VALUES) OR
              (WAFN-QTDE-OCOR          NOT NUMERIC)                OR
              (WAFN-QTDE-TOT-REG       NOT NUMERIC)                OR
              (WAFN-FUNC-BDSCO         EQUAL SPACES OR LOW-VALUES) OR
              (WAFN-AGPTO              NOT NUMERIC)                OR
              (WAFN-AGPTO              EQUAL ZEROS)                OR
              (WAFN-COD-PACOTE         NOT NUMERIC)                OR
              (WAFN-FIM                NOT EQUAL 'N' AND 'S')
               MOVE 'S'                TO WAFO-FIM

               MOVE 1                  TO WAFO-ERRO
               MOVE ZEROS              TO WAFO-COD-SQL-ERRO
               MOVE 0001               TO WAFO-COD-MSG-ERRO

               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFO-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WAFN-QTDE-OCOR         TO WRK-QTDE-OCOR.

           MOVE WAFN-AGPTO             TO CAGPTO-CTA OF GFCTB0L0

           IF  WAFN-COD-PACOTE         EQUAL ZEROS
               PERFORM 2100-TRATAR-CURSOR-1
           ELSE
               MOVE WAFN-COD-PACOTE    TO CSERVC-TARIF OF GFCTB0L0

               PERFORM 2200-TRATAR-CURSOR-2
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-TRATAR-CURSOR-1            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.

           PERFORM 2110-ABRIR-CSR01-GFCTB0L0.

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER WRK-QTDE-OCOR
                OR WRK-FIM-CURSOR      EQUAL 'S'

                   PERFORM 2120-LER-CSR01-GFCTB0L0

           END-PERFORM.

           PERFORM 2120-LER-CSR01-GFCTB0L0.

           MOVE ZEROS                  TO IND-1.

           PERFORM 2130-PROCESSAR-CURSOR-1
             UNTIL IND-1               EQUAL 7
                OR WRK-FIM-CURSOR      EQUAL 'S'.

           PERFORM 2140-FECHAR-CSR01-GFCTB0L0.


           IF  WRK-FIM-CURSOR          EQUAL 'S'
               MOVE 'S'                TO WAFO-FIM

               MOVE WAFO-QTDE-OCOR     TO WRK-QTDE-OCOR

               IF  WRK-QTDE-OCOR       EQUAL ZEROS
                   MOVE 1              TO WAFO-ERRO
                   MOVE ZEROS          TO WAFO-COD-SQL-ERRO
                   MOVE 1576           TO WAFO-COD-MSG-ERRO

                   MOVE '1576'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFO-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.


      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ABRIR-CSR01-GFCTB0L0       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR01-GFCTB0L0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB

               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL

               MOVE 0005               TO WAFO-COD-MSG-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

       2120-LER-CSR01-GFCTB0L0         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH
                     CSR01-GFCTB0L0
               INTO
                    :GFCTB0L0.CAGPTO-CTA,
                    :GFCTB0L0.CSERVC-TARIF,
                    :GFCTB0L0.HINCL-REG,
                    :GFCTB0L0.CSIT-SOLTC-CANCT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB

               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL

               MOVE 0006               TO WAFO-COD-MSG-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*

       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2130-PROCESSAR-CURSOR-1         SECTION.
      *----------------------------------------------------------------*

           IF  CSERVC-TARIF OF GFCTB0L0
                                       NOT EQUAL WRK-CSERVC-TARIF
               PERFORM 2300-ACESSAR-GFCTB0D8

               MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO WRK-CSERVC-TARIF
           END-IF.

           ADD  +1                     TO IND-1.

           PERFORM 2400-MOVER-CAMPOS.

           PERFORM 2120-LER-CSR01-GFCTB0L0.


      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2140-FECHAR-CSR01-GFCTB0L0      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE  CSR01-GFCTB0L0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL

               MOVE '0030'             TO GFCT0M-LOCAL

               MOVE 0011               TO WAFO-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-TRATAR-CURSOR-2            SECTION.

      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.

           PERFORM 2210-ABRIR-CSR02-GFCTB0L0.

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER WRK-QTDE-OCOR
                OR WRK-FIM-CURSOR      EQUAL 'S'

                   PERFORM 2220-LER-CSR02-GFCTB0L0

           END-PERFORM.

           PERFORM 2220-LER-CSR02-GFCTB0L0.

           MOVE ZEROS                  TO IND-1.

           PERFORM 2230-PROCESSAR-CURSOR-2
             UNTIL IND-1               EQUAL 7

                OR WRK-FIM-CURSOR      EQUAL 'S'.

           PERFORM 2240-FECHAR-CSR02-GFCTB0L0.

           IF  WRK-FIM-CURSOR          EQUAL 'S'
               MOVE 'S'                TO WAFO-FIM

               MOVE WAFO-QTDE-OCOR     TO WRK-QTDE-OCOR

               IF  WRK-QTDE-OCOR       EQUAL ZEROS
                   MOVE 1              TO WAFO-ERRO
                   MOVE ZEROS          TO WAFO-COD-SQL-ERRO
                   MOVE 1576           TO WAFO-COD-MSG-ERRO

                   MOVE '1576'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFO-DESC-MSG-ERRO


                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-ABRIR-CSR02-GFCTB0L0       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR02-GFCTB0L0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL

               MOVE 0005               TO WAFO-COD-MSG-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-LER-CSR02-GFCTB0L0         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH
                     CSR02-GFCTB0L0
               INTO
                    :GFCTB0L0.CAGPTO-CTA,
                    :GFCTB0L0.CSERVC-TARIF,
                    :GFCTB0L0.HINCL-REG,
                    :GFCTB0L0.CSIT-SOLTC-CANCT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS


               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL

               MOVE 0006               TO WAFO-COD-MSG-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-PROCESSAR-CURSOR-2         SECTION.
      *----------------------------------------------------------------*

           IF  CSERVC-TARIF OF GFCTB0L0
                                       NOT EQUAL WRK-CSERVC-TARIF
               PERFORM 2300-ACESSAR-GFCTB0D8

               MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO WRK-CSERVC-TARIF
           END-IF.

           ADD  +1                     TO IND-1.

           PERFORM 2400-MOVER-CAMPOS.

           PERFORM 2220-LER-CSR02-GFCTB0L0.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-FECHAR-CSR02-GFCTB0L0      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE  CSR02-GFCTB0L0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL

               MOVE 0011               TO WAFO-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-ACESSAR-GFCTB0D8           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                     RSERVC_TARIF_REDZD
               INTO
                    :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM
                     DB2PRD.SERVC_TARIF_PRINC
              WHERE
                     CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')

               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFO-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0D8'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

           MOVE WAFO-QTDE-OCOR         TO WRK-WAFO-QTDE-OCOR.
           ADD  +1                     TO WRK-WAFO-QTDE-OCOR.
           MOVE WRK-WAFO-QTDE-OCOR     TO WAFO-QTDE-OCOR.

           MOVE WAFO-QTDE-TOT-REG      TO WRK-WAFO-QTDE-TOT-REG.
           ADD  +1                     TO WRK-WAFO-QTDE-TOT-REG.
           MOVE WRK-WAFO-QTDE-TOT-REG  TO WAFO-QTDE-TOT-REG.

           ADD  +123                   TO WAFO-LL.

           EVALUATE WAFN-AGPTO
              WHEN 15
              WHEN 17
              WHEN 20
                  PERFORM 2410-OBTER-PAB
              WHEN 16
                  PERFORM 2420-OBTER-EMPRESA
              WHEN 22
                  PERFORM 2430-OBTER-FORNEC
           END-EVALUATE.

           MOVE HINCL-REG OF GFCTB0L0  TO WAFO-HINCL(IND-1)
                                          WRK-HINCL.

           MOVE WRK-ANO-INCL           TO WRK-ANO-DB2.
           MOVE WRK-MES-INCL           TO WRK-MES-DB2.
           MOVE WRK-DIA-INCL           TO WRK-DIA-DB2.
           MOVE WRK-DATA-DB2           TO WAFO-DATA-SOLIC(IND-1).

           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO WRK-CSERVC-TARIF.
           MOVE WRK-COD-TARIFA         TO WAFO-PACOTE(IND-1).


           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8 (1:20)
                                       TO WAFO-NOME-PACOTE(IND-1).
           MOVE CSIT-SOLTC-CANCT OF GFCTB0L0
                                       TO WAFO-SITUACAO(IND-1).

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-OBTER-PAB                  SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L3.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L3.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L3.

           EXEC SQL
             SELECT
                     CDEPDC,
                     CPOSTO_SERVC
               INTO
                    :GFCTB0L3.CDEPDC,
                    :GFCTB0L3.CPOSTO-SERVC
               FROM
                     DB2PRD.TCANCT_ADSAO_POSTO
              WHERE
                     CAGPTO_CTA         = :GFCTB0L3.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L3.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L3.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL

               MOVE '0080'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFO-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L3'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           MOVE CDEPDC OF GFCTB0L3     TO WRK-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO WAFO-AGENCIA(IND-1).

           MOVE CPOSTO-SERVC OF GFCTB0L3
                                       TO WRK-POSTO.
           MOVE WRK-COD-POSTO          TO WAFO-POSTO(IND-1).

           IF  WAFN-AGPTO              EQUAL 015
               PERFORM 2411-OBTER-DESC-PAB
           ELSE
               PERFORM 2412-OBTER-DESC-CORRESP-BANC
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)   TO WAFO-NOME-POSTO(IND-1).

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2411-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGB-ENTRADA
                                       GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE WAFN-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAFN-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE WAFN-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE WRK-COD-AGENCIA        TO GFCTGB-COD-DEPDC.
           MOVE WRK-COD-POSTO          TO GFCTGB-COD-POSTO.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFO-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0146'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFO-FIM

               MOVE GFCTGC-ERRO        TO WAFO-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAFO-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAFO-COD-MSG-ERRO


               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE ZEROS          TO WAFO-COD-SQL-ERRO
                   MOVE 1577           TO WAFO-COD-MSG-ERRO

                   MOVE '1577'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109'
                          ' - '
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFO-DESC-MSG-ERRO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*

       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2412-OBTER-DESC-CORRESP-BANC    SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGB-ENTRADA
                                       GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE WAFN-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAFN-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE WAFN-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.

           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE WRK-COD-AGENCIA        TO GFCTGB-COD-DEPDC.
           MOVE WRK-COD-POSTO          TO GFCTGB-COD-POSTO.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFO-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0146'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF


           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAFO-FIM

               MOVE GFCTGC-ERRO        TO WAFO-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAFO-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAFO-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE ZEROS          TO WAFO-COD-SQL-ERRO
                   MOVE 1579           TO WAFO-COD-MSG-ERRO
                   MOVE '1579'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFO-DESC-MSG-ERRO
               ELSE

                   STRING 'GFCT5090'
                          ' - '
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFO-DESC-MSG-ERRO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2412-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2420-OBTER-EMPRESA              SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L1.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L1.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L1.


           EXEC SQL
             SELECT
                     CAG_BCRIA,
                     CCTA_BCRIA_CLI
               INTO
                    :GFCTB0L1.CAG-BCRIA,
                    :GFCTB0L1.CCTA-BCRIA-CLI
               FROM
                     DB2PRD.TCANCT_ADSAO_EMPR
              WHERE
                     CAGPTO_CTA         = :GFCTB0L1.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L1.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L1.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_EMPR'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFO-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L1'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           MOVE CAG-BCRIA OF GFCTB0L1  TO WRK-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO WAFO-AGENCIA(IND-1).

           MOVE CCTA-BCRIA-CLI OF GFCTB0L1
                                       TO WRK-CONTA.
           MOVE WRK-COD-CONTA          TO WAFO-CONTA(IND-1).

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2430-OBTER-FORNEC               SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L2.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L2.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L2.

           EXEC SQL
             SELECT
ST2506*              CCNPJ_CPF,
ST2506*              CFLIAL_CNPJ,
ST2506*              CCTRL_CNPJ
ST2506               CCNPJ_CPF_ST,
ST2506               CFLIAL_CNPJ_ST,
ST2506               CCTRL_CNPJ_ST
               INTO
ST2506*             :GFCTB0L2.CCNPJ-CPF,
ST2506*             :GFCTB0L2.CFLIAL-CNPJ,
ST2506*             :GFCTB0L2.CCTRL-CNPJ
ST2506              :GFCTB0L2.CCNPJ-CPF-ST,
ST2506              :GFCTB0L2.CFLIAL-CNPJ-ST,
ST2506              :GFCTB0L2.CCTRL-CNPJ-ST
               FROM
                     DB2PRD.TCANCT_ADSAO_FORNC
              WHERE
                     CAGPTO_CTA         = :GFCTB0L2.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L2.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L2.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_FORNC'

                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFO-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L2'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFO-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

ST2506*    MOVE CCNPJ-CPF OF GFCTB0L2  TO WRK-CCNPJ-CPF.
ST2506*    MOVE WRK-COD-CNPJ           TO WAFO-CNPJ-NUM(IND-1).
ST2506     MOVE CCNPJ-CPF-ST           OF GFCTB0L2  
ST2506                                 TO WAFO-CNPJ-CGC(IND-1).

ST2506*    MOVE CFLIAL-CNPJ OF GFCTB0L2
ST2506*
ST2506*                                TO WRK-CFLIAL-CNPJ.
ST2506*    MOVE WRK-COD-FILIAL         TO WAFO-CNPJ-FIL(IND-1)
ST2506     MOVE CFLIAL-CNPJ-ST         OF GFCTB0L2
ST2506                                 TO WAFO-CNPJ-FIL(IND-1).
ST2506*    MOVE CCTRL-CNPJ OF GFCTB0L2 TO WRK-CCTRL-CNPJ.
ST2506     MOVE CCTRL-CNPJ-ST OF GFCTB0L2 TO WRK-CCTRL-CNPJ.
           MOVE WRK-COD-CONTROLE       TO WAFO-CNPJ-CTR(IND-1).

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9900-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO WAFO-ERRO.
           MOVE 'GFCT0146'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          WRK-SQLCODE.


           MOVE WRK-COD-SQL            TO WAFO-COD-SQL-ERRO

           MOVE SPACES                 TO WAFO-DESC-MSG-ERRO.

           PERFORM 9990-OBTER-DESC-MENSAGEM.

           STRING GFCTG3-DESC-MSG
                  ' - '
                  WRK-COD-TABELA
           DELIMITED BY '  '           INTO WAFO-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9990-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.

           MOVE WAFN-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAFN-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAFN-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0146'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR

           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO WAFO-ERRO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0146'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
