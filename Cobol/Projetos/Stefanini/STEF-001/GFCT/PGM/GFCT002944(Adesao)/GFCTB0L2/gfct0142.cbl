      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0142.
       AUTHOR.     SONDAPROCWORK OUTSOURCING.
      *================================================================*
      *        S O N D A P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0142                                     *
      *    ANALISTA     : WAGNER SILVA    - PROCWORK  - GRUPO 50       *
      *    DATA         : 19/03/2009                                   *
      *                                                                *
      *    OBJETIVO     : INCLUSAO DA SOLICITACAO DE EXCLUSAO MASSIVA  *
      *                   DE ADESAO.                                   *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.TCANCT_ADSAO_AGPTO               GFCTB0L0          *
      *      DB2PRD.TCANCT_ADSAO_EMPR                GFCTB0L1          *
      *      DB2PRD.TCANCT_ADSAO_FORNC               GFCTB0L2          *
      *      DB2PRD.TCANCT_ADSAO_POSTO               GFCTB0L3          *
      *      DB2PRD.CLIENTE_PAGADOR                  PFEBB000          *
      *      DB2PRD.TCAD_CORSP_BCRIO                 YMKCB001          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5522 - VERIFICAR ON-LINE                              *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5089 - OBTER NOME DO CLIENTE POR AGENCIA/CONTA        *
      *      MESU9015 - CONSISTIR AGENCIA                              *
      *      MESU9018 - CONSISTIR PAB                                  *
      *                                                                *
      *    NAVEGACAO:                                                  *
      *      CHAMADO POR: GFCT7148                                     *
      *                                                                *
      *================================================================*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  05 / 2014                                    *
      *    OBJETIVO....:  SUBSTITUIR O ACESSO A BASE YMKCB001          *
      *                   PELO MODULO KCIM0184.                        *
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
       01  FILLER                      PIC  X(032)         VALUE
           '*  INICIO DA WORKING GFCT0142  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       01  WRK-FIM-CURSOR-GFCTB0L0     PIC  X(001)         VALUE SPACES.
       01  WRK-ACHOU-DUPLICIDADE       PIC  X(001)         VALUE SPACES.

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-COD-TABELA              PIC  X(008)         VALUE SPACES.

       01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE.
           05  FILLER                  PIC  9(006).
           05  WRK-COD-SQL             PIC  9(003).

       01  WRK-FUNC-BDSCO              PIC  9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-FUNC-BDSCO.
           05  FILLER                  PIC  9(002).
           05  WRK-WAFF-FUNC-BDSCO     PIC  X(007).

       01  WRK-WAFF-CCTA               PIC  9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-WAFF-CCTA.
           05  FILLER                  PIC  9(006).
           05  WRK-COD-CONTA           PIC  9(007).

       01  WRK-TIMESTAMP               PIC  X(026)         VALUE SPACES.

       01  WRK-TIMESTAMP-INI           PIC  X(026)         VALUE
           '0001-01-01-01.01.01.000001'.

       01  WRK-CPAB-9-05               PIC  9(005)       VALUE ZEROS.
       01  WRK-FILLER                  REDEFINES WRK-CPAB-9-05.
           05 FILLER                   PIC  9(02).
           05 WRK-CPAB-9-03            PIC  9(03).

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
           '*    AREA DO MODULO GFCT5089   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTQV'.
       COPY 'I#GFCTWW'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO MESU9015   *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO MESU9018   *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*      AREA DE TABELAS DB2     *'.
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
                INCLUDE PFEBB000
           END-EXEC.

           EXEC SQL
            DECLARE  CSR01-GFCTB0L0    CURSOR WITH HOLD FOR
             SELECT
                     CAGPTO_CTA      ,
                     CSERVC_TARIF    ,
                     HINCL_REG       ,
                     CSIT_SOLTC_CANCT
               FROM
                     DB2PRD.TCANCT_ADSAO_AGPTO
              WHERE
                     CAGPTO_CTA        = :GFCTB0L0.CAGPTO-CTA
                AND  CSERVC_TARIF      = :GFCTB0L0.CSERVC-TARIF
                AND  CSIT_SOLTC_CANCT IN (01, 02, 04)
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    FIM DA WORKING GFCT0142   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY KCIMW184.
       COPY GFCTWAFF.
       COPY GFCTWAFG.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING WAFF-ENTRADA
                                                      WAFG-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

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

           MOVE SPACES                 TO WAFG-SAIDA.

           INITIALIZE                  WAFG-SAIDA.

           MOVE +1010                  TO WAFG-LL.
           MOVE WAFF-TRANSACAO         TO WAFG-TRANSACAO.
           MOVE WAFF-FUNCAO            TO WAFG-FUNCAO.

           MOVE WAFF-FUNC-BDSCO        TO WAFG-FUNC-BDSCO.
           MOVE WAFF-TERMINAL          TO WAFG-TERMINAL.
           MOVE 'S'                    TO WAFG-FIM.

           MOVE ZEROS                  TO WAFG-ERRO
                                          WAFG-COD-SQL-ERRO
                                          WAFG-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 9990-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WAFG-DESC-MSG-ERRO.

           MOVE WAFF-FIXO              TO WAFG-FIXO.

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
           MOVE WAFF-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAFF-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAFF-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFG-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0142'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 9                  TO WAFG-ERRO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG1-COD-MSG-ERRO
                                       TO WAFG-COD-MSG-ERRO
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAFG-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAFG-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0142'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAFG-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0142'         TO GFCT0M-TRANSACAO
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

           IF (WAFF-TRANSACAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAFF-FUNCAO             EQUAL SPACES OR LOW-VALUES) OR
              (WAFF-FUNC-BDSCO         EQUAL SPACES OR LOW-VALUES) OR
              (WAFF-TERMINAL           EQUAL LOW-VALUES)           OR
              (WAFF-FIM                NOT EQUAL 'N' AND 'S')      OR
              (WAFF-CAGPTO             NOT EQUAL 15 AND 16 AND
                                                 17 AND 20 AND
                                                 22)               OR
              (WAFF-CTARIF             NOT NUMERIC)                OR
              (WAFF-CTARIF             EQUAL ZEROS)                OR
              (WAFF-CAGEN              NOT NUMERIC)                OR
              (WAFF-CPAB               NOT NUMERIC)                OR
              (WAFF-CCTA               NOT NUMERIC)                OR
              (WAFF-CCPF-NUM           NOT NUMERIC)                OR
              (WAFF-CCPF-FIL           NOT NUMERIC)                OR
              (WAFF-CCPF-CTR           NOT NUMERIC)                OR
              (WAFF-JUSTIF             EQUAL SPACES OR LOW-VALUES)
               MOVE 1                  TO WAFG-ERRO
               MOVE ZEROS              TO WAFG-COD-SQL-ERRO
               MOVE 0001               TO WAFG-COD-MSG-ERRO

               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFG-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WAFF-CAGPTO             EQUAL 15 OR
               WAFF-CAGPTO             EQUAL 16 OR
               WAFF-CAGPTO             EQUAL 17 OR
               WAFF-CAGPTO             EQUAL 20
               IF  WAFF-CAGEN          EQUAL ZEROS
                   MOVE 1              TO WAFG-ERRO
                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 0001           TO WAFG-COD-MSG-ERRO

                   MOVE '0001'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  WAFF-CAGPTO             EQUAL 15 OR
               WAFF-CAGPTO             EQUAL 17 OR
               WAFF-CAGPTO             EQUAL 20
               IF  WAFF-CPAB           EQUAL ZEROS
                   MOVE 1              TO WAFG-ERRO
                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 0001           TO WAFG-COD-MSG-ERRO

                   MOVE '0001'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  WAFF-CAGPTO             EQUAL 16
               IF  WAFF-CCTA           EQUAL ZEROS
                   MOVE 1              TO WAFG-ERRO
                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 0001           TO WAFG-COD-MSG-ERRO

                   MOVE '0001'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  WAFF-CAGPTO             EQUAL 22
               IF  WAFF-CCPF-NUM       EQUAL ZEROS
                   MOVE 1              TO WAFG-ERRO
                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 0001           TO WAFG-COD-MSG-ERRO

                   MOVE '0001'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE WAFF-CAGPTO
               WHEN 15

                    PERFORM 2100-CONSISTIR-AGENCIA

                    PERFORM 2200-CONSISTIR-POSTO

               WHEN 16

                    PERFORM 2100-CONSISTIR-AGENCIA

                    PERFORM 2300-CONSISTIR-EMPRESA

               WHEN 17
               WHEN 20

                    PERFORM 2100-CONSISTIR-AGENCIA

                    PERFORM 2400-CONSISTIR-CORRESPONDENTE

               WHEN 22

                    PERFORM 2500-CONSISTIR-FORNECEDOR

           END-EVALUATE.

           PERFORM 2700-VERIFICAR-DUPLICIDADE.

           PERFORM 2800-GERAR-SOLIC-CANC-ADESAO.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-CONSISTIR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO COMU-PARM
                                          WRK-POOL7100
                                          WRK-SQLCA.

           INITIALIZE                  COMU-PARM
                                       WRK-POOL7100
                                       WRK-SQLCA.

           MOVE 'GFCT0142'             TO COMU-NOME-PGM.
           MOVE 237                    TO COMU-BANCO.
           MOVE WAFF-CAGEN             TO COMU-DEPDC.

           MOVE 'MESU9015'             TO WRK-MODULO.

           CALL WRK-MODULO             USING COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  COMU-COD-RETORNO        NOT EQUAL ZEROS
               MOVE 1                  TO WAFG-ERRO

               MOVE ZEROS              TO WAFG-COD-SQL-ERRO
               MOVE 1560               TO WAFG-COD-MSG-ERRO

               MOVE '1560'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFG-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-CONSISTIR-POSTO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2600-CONSISTIR-CODIGO-PAB.

           IF  COMU-COD-TIPO-POSTO     EQUAL 9
               MOVE 1                  TO WAFG-ERRO

               MOVE ZEROS              TO WAFG-COD-SQL-ERRO
               MOVE 1561               TO WAFG-COD-MSG-ERRO

               MOVE '1561'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFG-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-CONSISTIR-EMPRESA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTQV-ENTRADA
                                          GFCTWW-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTQV-ENTRADA
                                       GFCTWW-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTQV-LL.
           MOVE ZEROS                  TO GFCTQV-ZZ.

           MOVE WAFF-TRANSACAO         TO GFCTQV-TRANSACAO.
           MOVE WAFF-FUNCAO            TO GFCTQV-FUNCAO.
           MOVE WAFF-FUNC-BDSCO        TO GFCTQV-FUNC-BDSCO.
           MOVE WAFF-CAGEN             TO GFCTQV-COD-DEPDC.

           MOVE WAFF-CCTA              TO WRK-WAFF-CCTA.
           MOVE WRK-COD-CONTA          TO GFCTQV-CCTA-CLI.

           MOVE 'N'                    TO GFCTQV-FIM.

           MOVE 'GFCT5089'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTQV-ENTRADA
                                             GFCTWW-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFG-ERRO

               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0142'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF

           IF  GFCTWW-ERRO             EQUAL ZEROS
               IF  GFCTWW-CTPO-PSSOA   NOT EQUAL 'J'
                   MOVE 1              TO WAFG-ERRO

                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 1570           TO WAFG-COD-MSG-ERRO

                   MOVE '1570'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               END-IF
           ELSE
               MOVE GFCTWW-ERRO        TO WAFG-ERRO
               MOVE GFCTWW-COD-SQL-ERRO
                                       TO WAFG-COD-SQL-ERRO
               MOVE GFCTWW-COD-MSG-ERRO
                                       TO WAFG-COD-MSG-ERRO

               IF  GFCTWW-ERRO         EQUAL 1
                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 1562           TO WAFG-COD-MSG-ERRO

                   MOVE '1562'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5089'
                          ' - '
                          GFCTWW-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAFG-DESC-MSG-ERRO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-CONSISTIR-CORRESPONDENTE   SECTION.
      *----------------------------------------------------------------*

           PERFORM 2600-CONSISTIR-CODIGO-PAB.

           IF  COMU-COD-TIPO-POSTO     NOT EQUAL 9
               IF  WAFF-CAGPTO         EQUAL 17
                   MOVE 1              TO WAFG-ERRO

                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 1563           TO WAFG-COD-MSG-ERRO

                   MOVE '1563'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               ELSE
                   MOVE 1              TO WAFG-ERRO

                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 1564           TO WAFG-COD-MSG-ERRO

                   MOVE '1564'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           PERFORM 2410-VALIDAR-CORRESPONDENTE.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-VALIDAR-CORRESPONDENTE     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE KCIMW184-AREA-COMUNICACAO.

           MOVE 01                     TO KCIMW184-E-FUNCAO.
           MOVE WAFF-CAGEN             TO KCIMW184-E-CAG-BCRIA-1.
           MOVE WAFF-CPAB              TO WRK-CPAB-9-05.
           MOVE WRK-CPAB-9-03          TO KCIMW184-E-CPOSTO-BCRIO-1.
           MOVE 'KCIM0184'             TO WRK-MODULO.

           CALL WRK-MODULO             USING KCIMW184-AREA-COMUNICACAO.

            IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
               MOVE 1                  TO WAFG-ERRO
               MOVE ZEROS              TO WAFG-COD-SQL-ERRO
               MOVE 1875               TO WAFG-COD-MSG-ERRO
               MOVE '1875'             TO GFCTG2-COD-MSG
               PERFORM 9990-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WAFG-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  KCIMW184-S-COD-RETORNO EQUAL 98
               IF  WAFF-CAGPTO         EQUAL 17
                   MOVE 1              TO WAFG-ERRO

                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 1565           TO WAFG-COD-MSG-ERRO

                   MOVE '1565'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               ELSE
                   MOVE 1              TO WAFG-ERRO

                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 1566           TO WAFG-COD-MSG-ERRO

                   MOVE '1566'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  KCIMW184-S-CEMPR-CORSP(1)  NOT EQUAL 10000 AND
               WAFF-CAGPTO             EQUAL 17
               MOVE 1                  TO WAFG-ERRO

               MOVE ZEROS              TO WAFG-COD-SQL-ERRO
               MOVE 1563               TO WAFG-COD-MSG-ERRO

               MOVE '1563'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFG-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  KCIMW184-S-CEMPR-CORSP(1) EQUAL  10000 AND
               WAFF-CAGPTO                 EQUAL 20
               MOVE 1                  TO WAFG-ERRO

               MOVE ZEROS              TO WAFG-COD-SQL-ERRO
               MOVE 1564               TO WAFG-COD-MSG-ERRO

               MOVE '1564'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFG-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-CONSISTIR-FORNECEDOR       SECTION.
      *----------------------------------------------------------------*

ST2506*    M0VE WAFF-CCPF-NUM          TO CCGC-CPF   OF PFEBB000.
ST2506*    M0VE WAFF-CCPF-FIL          TO CFLIAL-CGC OF PFEBB000.
ST2506*    M0VE WAFF-CCPF-CTR          TO CCTRL-CGC  OF PFEBB000.
ST2506     MOVE WAFF-CCPF-NUM          TO CCGC-CPF   OF PFEBB000.
ST2506     MOVE WAFF-CCPF-FIL          TO CFLIAL-CGC OF PFEBB000.
ST2506     MOVE WAFF-CCPF-CTR          TO CCTRL-CGC  OF PFEBB000.

           EXEC SQL
             SELECT
                     CCGC_CPF
               INTO
                    :PFEBB000.CCGC-CPF
               FROM
                     DB2PRD.CLIENTE_PAGADOR
              WHERE
                     CCGC_CPF           = :PFEBB000.CCGC-CPF
                AND  CFLIAL_CGC         = :PFEBB000.CFLIAL-CGC
                AND  CCTRL_CGC          = :PFEBB000.CCTRL-CGC
042009          AND  CINDCD_PROD_TESTE  = 'P'
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'CLIENTE_PAGADOR'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFG-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'PFEBB000'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WAFG-ERRO

               MOVE ZEROS              TO WAFG-COD-SQL-ERRO
               MOVE 1567               TO WAFG-COD-MSG-ERRO

               MOVE '1567'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFG-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-CONSISTIR-CODIGO-PAB       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO COMU-DADOS-9018.

           INITIALIZE                  COMU-DADOS-9018.

           MOVE 'VRS001'               TO COMU-VERSAO.
           MOVE 237                    TO COMU-COD-EMPR.
           MOVE WAFF-CAGEN             TO COMU-COD-DEPDC.
           MOVE WAFF-CPAB              TO COMU-COD-POSTO.
           MOVE SPACES                 TO COMU-CORR-BANCARIO.

           MOVE 'MESU9018'             TO WRK-MODULO.

           CALL WRK-MODULO             USING COMU-DADOS-9018.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               IF  RETURN-CODE         EQUAL 4
                   MOVE 1              TO WAFG-ERRO

                   MOVE ZEROS          TO WAFG-COD-SQL-ERRO
                   MOVE 0518           TO WAFG-COD-MSG-ERRO

                   MOVE '0518'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR
               ELSE
                   IF  RETURN-CODE     EQUAL 8
                       MOVE 1          TO WAFG-ERRO

                       MOVE ZEROS      TO WAFG-COD-SQL-ERRO
                       MOVE 0519       TO WAFG-COD-MSG-ERRO

                       MOVE '0519'     TO GFCTG2-COD-MSG

                       PERFORM 9990-OBTER-DESC-MENSAGEM

                       MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                       PERFORM 9999-FINALIZAR
                   ELSE
                       IF  RETURN-CODE EQUAL 12
                           MOVE 1      TO WAFG-ERRO

                           MOVE ZEROS  TO WAFG-COD-SQL-ERRO
                           MOVE 0520   TO WAFG-COD-MSG-ERRO

                           MOVE '0520' TO GFCTG2-COD-MSG

                           PERFORM 9990-OBTER-DESC-MENSAGEM

                           MOVE GFCTG3-DESC-MSG
                                       TO WAFG-DESC-MSG-ERRO

                           PERFORM 9999-FINALIZAR
                       END-IF
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-VERIFICAR-DUPLICIDADE      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR-GFCTB0L0.

           MOVE 'N'                    TO WRK-ACHOU-DUPLICIDADE.

           MOVE WAFF-CAGPTO            TO CAGPTO-CTA   OF GFCTB0L0.
           MOVE WAFF-CTARIF            TO CSERVC-TARIF OF GFCTB0L0.

           PERFORM 2710-ABRIR-CURSOR-GFCTB0L0.

           PERFORM 2720-LER-CURSOR-GFCTB0L0.

           PERFORM 2730-PROCESSAR-CURSOR-GFCTB0L0
             UNTIL WRK-FIM-CURSOR-GFCTB0L0
                                       EQUAL 'S'
                OR WRK-ACHOU-DUPLICIDADE
                                       EQUAL 'S'.

           PERFORM 2740-FECHAR-CURSOR-GFCTB0L0.

           IF  WRK-ACHOU-DUPLICIDADE   EQUAL 'S'
               MOVE 1                  TO WAFG-ERRO

               MOVE ZEROS              TO WAFG-COD-SQL-ERRO
               MOVE 1568               TO WAFG-COD-MSG-ERRO

               MOVE '1568'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAFG-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2710-ABRIR-CURSOR-GFCTB0L0      SECTION.
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
               MOVE '0030'             TO GFCT0M-LOCAL

               MOVE 0005               TO WAFG-COD-MSG-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2720-LER-CURSOR-GFCTB0L0        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH  CSR01-GFCTB0L0

               INTO
                    :GFCTB0L0.CAGPTO-CTA      ,
                    :GFCTB0L0.CSERVC-TARIF    ,
                    :GFCTB0L0.HINCL-REG       ,
                    :GFCTB0L0.CSIT-SOLTC-CANCT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL

               MOVE 0006               TO WAFG-COD-MSG-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-GFCTB0L0
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2730-PROCESSAR-CURSOR-GFCTB0L0  SECTION.
      *----------------------------------------------------------------*

           EVALUATE WAFF-CAGPTO
               WHEN 15
               WHEN 17
               WHEN 20

                    PERFORM 2731-VERIFICAR-DUPLIC-POSTO

               WHEN 16

                    PERFORM 2732-VERIFICAR-DUPLIC-EMPRESA

               WHEN 22

                    PERFORM 2733-VERIFICAR-DUPLIC-FORNEC

           END-EVALUATE.

           PERFORM 2720-LER-CURSOR-GFCTB0L0.

      *----------------------------------------------------------------*
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2731-VERIFICAR-DUPLIC-POSTO     SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA   OF GFCTB0L3.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF OF GFCTB0L3.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG    OF GFCTB0L3.
           MOVE 237                    TO CEMPR-INC    OF GFCTB0L3.
           MOVE WAFF-CAGEN             TO CDEPDC       OF GFCTB0L3.
           MOVE WAFF-CPAB              TO CPOSTO-SERVC OF GFCTB0L3.

           EXEC SQL
             SELECT
                     CEMPR_INC   ,
                     CDEPDC      ,
                     CPOSTO_SERVC
               INTO
                    :GFCTB0L3.CEMPR-INC   ,
                    :GFCTB0L3.CDEPDC      ,
                    :GFCTB0L3.CPOSTO-SERVC
               FROM
                     DB2PRD.TCANCT_ADSAO_POSTO
              WHERE
                     CAGPTO_CTA         = :GFCTB0L3.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L3.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L3.HINCL-REG
                AND  CEMPR_INC          = :GFCTB0L3.CEMPR-INC
                AND  CDEPDC             = :GFCTB0L3.CDEPDC
                AND  CPOSTO_SERVC       = :GFCTB0L3.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFG-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L3'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       2731-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2732-VERIFICAR-DUPLIC-EMPRESA   SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA     OF GFCTB0L1.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF   OF GFCTB0L1.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG      OF GFCTB0L1.
           MOVE 237                    TO CBCO           OF GFCTB0L1.
           MOVE WAFF-CAGEN             TO CAG-BCRIA      OF GFCTB0L1.
           MOVE WAFF-CCTA              TO CCTA-BCRIA-CLI OF GFCTB0L1.

           EXEC SQL
             SELECT
                     CBCO,
                     CAG_BCRIA,
                     CCTA_BCRIA_CLI
               INTO
                    :GFCTB0L1.CBCO,
                    :GFCTB0L1.CAG-BCRIA,
                    :GFCTB0L1.CCTA-BCRIA-CLI
               FROM
                     DB2PRD.TCANCT_ADSAO_EMPR
              WHERE
                     CAGPTO_CTA         = :GFCTB0L1.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L1.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L1.HINCL-REG
                AND  CBCO               = :GFCTB0L1.CBCO
                AND  CAG_BCRIA          = :GFCTB0L1.CAG-BCRIA
                AND  CCTA_BCRIA_CLI     = :GFCTB0L1.CCTA-BCRIA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_EMPR'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFG-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L1'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       2732-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2733-VERIFICAR-DUPLIC-FORNEC    SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L0 TO CAGPTO-CTA    OF GFCTB0L2.
           MOVE CSERVC-TARIF OF GFCTB0L0
                                       TO CSERVC-TARIF  OF GFCTB0L2.
           MOVE HINCL-REG OF GFCTB0L0  TO HINCL-REG     OF GFCTB0L2.
           MOVE WAFF-CCPF-NUM          TO CCNPJ-CPF     OF GFCTB0L2.
           MOVE WAFF-CCPF-FIL          TO CFLIAL-CNPJ   OF GFCTB0L2.
           MOVE WAFF-CCPF-CTR          TO CCTRL-CNPJ    OF GFCTB0L2.

           EXEC SQL
             SELECT
                     CCNPJ_CPF  ,
                     CFLIAL_CNPJ,
                     CCTRL_CNPJ
               INTO
                    :GFCTB0L2.CCNPJ-CPF  ,
                    :GFCTB0L2.CFLIAL-CNPJ,
                    :GFCTB0L2.CCTRL-CNPJ
               FROM
                     DB2PRD.TCANCT_ADSAO_FORNC
              WHERE
                     CAGPTO_CTA         = :GFCTB0L2.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0L2.CSERVC-TARIF
                AND  HINCL_REG          = :GFCTB0L2.HINCL-REG
                AND  CCNPJ_CPF          = :GFCTB0L2.CCNPJ-CPF
                AND  CFLIAL_CNPJ        = :GFCTB0L2.CFLIAL-CNPJ
                AND  CCTRL_CNPJ         = :GFCTB0L2.CCTRL-CNPJ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_FORNC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL

               MOVE 0012               TO WAFG-COD-MSG-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L2'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       2733-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2740-FECHAR-CURSOR-GFCTB0L0     SECTION.
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
               MOVE '0080'             TO GFCT0M-LOCAL

               MOVE 0011               TO WAFG-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA
               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2800-GERAR-SOLIC-CANC-ADESAO    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SET :WRK-TIMESTAMP      = CURRENT TIMESTAMP
           END-EXEC.

           PERFORM 2810-GERAR-CANC-ADESAO-AGRUP.

           EVALUATE WAFF-CAGPTO
               WHEN 15
               WHEN 17
               WHEN 20

                    PERFORM 2820-GERAR-CANC-ADESAO-POSTO

               WHEN 16

                    PERFORM 2830-GERAR-CANC-ADESAO-EMPR

               WHEN 22

                    PERFORM 2840-GERAR-CANC-ADESAO-FORNEC

           END-EVALUATE.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2810-GERAR-CANC-ADESAO-AGRUP    SECTION.
      *----------------------------------------------------------------*

           MOVE WAFF-CAGPTO            TO CAGPTO-CTA        OF GFCTB0L0.
           MOVE WAFF-CTARIF            TO CSERVC-TARIF      OF GFCTB0L0.

           MOVE WAFF-FUNC-BDSCO        TO WRK-WAFF-FUNC-BDSCO.
           MOVE WRK-FUNC-BDSCO         TO CFUNC-SOLTC-CANCT OF GFCTB0L0.

           MOVE ZEROS                  TO CFUNC-MANUT       OF GFCTB0L0.
           MOVE 1                      TO CSIT-SOLTC-CANCT  OF GFCTB0L0.
           MOVE ZEROS                  TO QPRVIO-CANCT-ADSAO
                                                            OF GFCTB0L0
                                          QADSAO-CANCD      OF GFCTB0L0.

           MOVE WAFF-JUSTIF            TO RJUSTF-SOLTC-CANCT-TEXT
                                                            OF GFCTB0L0.
           MOVE LENGTH OF RJUSTF-SOLTC-CANCT-TEXT OF GFCTB0L0
                                       TO RJUSTF-SOLTC-CANCT-LEN
                                                            OF GFCTB0L0.

           MOVE SPACES                 TO RJUSTF-PAREC-CANCT
                                                            OF GFCTB0L0.
           MOVE LENGTH OF RJUSTF-PAREC-CANCT-TEXT OF GFCTB0L0
                                       TO RJUSTF-PAREC-CANCT-LEN
                                                            OF GFCTB0L0.

           PERFORM 2811-GRAVAR-CANC-ADESAO-AGRUP.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2811-GRAVAR-CANC-ADESAO-AGRUP   SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             INSERT
               INTO
                     DB2PRD.TCANCT_ADSAO_AGPTO

                    (CAGPTO_CTA        ,
                     CSERVC_TARIF      ,
                     HINCL_REG         ,
                     CFUNC_SOLTC_CANCT ,
                     CFUNC_MANUT       ,
                     CSIT_SOLTC_CANCT  ,
                     QPRVIO_CANCT_ADSAO,
                     QADSAO_CANCD      ,
                     HEXCUC_CALC_PRVIO ,
                     HEXCUC_PROCM      ,
                     HMANUT_REG        ,
                     RJUSTF_SOLTC_CANCT,
                     RJUSTF_PAREC_CANCT)
             VALUES
                    (:GFCTB0L0.CAGPTO-CTA        ,
                     :GFCTB0L0.CSERVC-TARIF      ,
                     :WRK-TIMESTAMP              ,
                     :GFCTB0L0.CFUNC-SOLTC-CANCT ,
                     :GFCTB0L0.CFUNC-MANUT       ,
                     :GFCTB0L0.CSIT-SOLTC-CANCT  ,
                     :GFCTB0L0.QPRVIO-CANCT-ADSAO,
                     :GFCTB0L0.QADSAO-CANCD      ,
                     :WRK-TIMESTAMP-INI          ,
                     :WRK-TIMESTAMP-INI          ,
                     :WRK-TIMESTAMP-INI          ,
                     :GFCTB0L0.RJUSTF-SOLTC-CANCT,
                     :GFCTB0L0.RJUSTF-PAREC-CANCT)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL

               MOVE 0003               TO WAFG-COD-MSG-ERRO

               MOVE '0003'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L0'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2811-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2820-GERAR-CANC-ADESAO-POSTO    SECTION.
      *----------------------------------------------------------------*

           MOVE WAFF-CAGPTO            TO CAGPTO-CTA   OF GFCTB0L3.
           MOVE WAFF-CTARIF            TO CSERVC-TARIF OF GFCTB0L3.
           MOVE 237                    TO CEMPR-INC    OF GFCTB0L3.
           MOVE WAFF-CAGEN             TO CDEPDC       OF GFCTB0L3.
           MOVE WAFF-CPAB              TO CPOSTO-SERVC OF GFCTB0L3.

           PERFORM 2821-GRAVAR-CANC-ADESAO-POSTO.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2821-GRAVAR-CANC-ADESAO-POSTO   SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             INSERT
               INTO
                     DB2PRD.TCANCT_ADSAO_POSTO

                    (CAGPTO_CTA  ,
                     CSERVC_TARIF,
                     HINCL_REG   ,
                     CEMPR_INC   ,
                     CDEPDC      ,
                     CPOSTO_SERVC)
             VALUES
                    (:GFCTB0L3.CAGPTO-CTA  ,
                     :GFCTB0L3.CSERVC-TARIF,
                     :WRK-TIMESTAMP        ,
                     :GFCTB0L3.CEMPR-INC   ,
                     :GFCTB0L3.CDEPDC      ,
                     :GFCTB0L3.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL

               MOVE 0003               TO WAFG-COD-MSG-ERRO

               MOVE '0003'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L3'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2821-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2830-GERAR-CANC-ADESAO-EMPR    SECTION.
      *----------------------------------------------------------------*

           MOVE WAFF-CAGPTO            TO CAGPTO-CTA     OF GFCTB0L1.
           MOVE WAFF-CTARIF            TO CSERVC-TARIF   OF GFCTB0L1.
           MOVE 237                    TO CBCO           OF GFCTB0L1.
           MOVE WAFF-CAGEN             TO CAG-BCRIA      OF GFCTB0L1.
           MOVE WAFF-CCTA              TO CCTA-BCRIA-CLI OF GFCTB0L1.

           PERFORM 2831-GRAVAR-CANC-ADESAO-EMPR.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2831-GRAVAR-CANC-ADESAO-EMPR    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             INSERT
               INTO
                     DB2PRD.TCANCT_ADSAO_EMPR

                    (CAGPTO_CTA    ,
                     CSERVC_TARIF  ,
                     HINCL_REG     ,
                     CBCO          ,
                     CAG_BCRIA     ,
                     CCTA_BCRIA_CLI)
             VALUES
                    (:GFCTB0L1.CAGPTO-CTA    ,
                     :GFCTB0L1.CSERVC-TARIF  ,
                     :WRK-TIMESTAMP          ,
                     :GFCTB0L1.CBCO          ,
                     :GFCTB0L1.CAG-BCRIA     ,
                     :GFCTB0L1.CCTA-BCRIA-CLI)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_EMPR'
                                       TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL

               MOVE 0003               TO WAFG-COD-MSG-ERRO

               MOVE '0003'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L1'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2831-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2840-GERAR-CANC-ADESAO-FORNEC   SECTION.
      *----------------------------------------------------------------*

           MOVE WAFF-CAGPTO            TO CAGPTO-CTA   OF GFCTB0L2.
           MOVE WAFF-CTARIF            TO CSERVC-TARIF OF GFCTB0L2.
           MOVE WAFF-CCPF-NUM          TO CCNPJ-CPF    OF GFCTB0L2.
           MOVE WAFF-CCPF-FIL          TO CFLIAL-CNPJ  OF GFCTB0L2.
           MOVE WAFF-CCPF-CTR          TO CCTRL-CNPJ   OF GFCTB0L2.

           PERFORM 2841-GRAVAR-CANC-ADESAO-FORNEC.

      *----------------------------------------------------------------*
       2840-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2841-GRAVAR-CANC-ADESAO-FORNEC  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             INSERT
               INTO
                     DB2PRD.TCANCT_ADSAO_FORNC

                    (CAGPTO_CTA  ,
                     CSERVC_TARIF,
                     HINCL_REG   ,
                     CCNPJ_CPF   ,
                     CFLIAL_CNPJ ,
                     CCTRL_CNPJ)
             VALUES
                    (:GFCTB0L2.CAGPTO-CTA  ,
                     :GFCTB0L2.CSERVC-TARIF,
                     :WRK-TIMESTAMP        ,
                     :GFCTB0L2.CCNPJ-CPF   ,
                     :GFCTB0L2.CFLIAL-CNPJ ,
                     :GFCTB0L2.CCTRL-CNPJ)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0M-AREA-ERROS

               MOVE 'TCANCT_ADSAO_FORNC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL

               MOVE 0003               TO WAFG-COD-MSG-ERRO

               MOVE '0003'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0L2'         TO WRK-COD-TABELA

               PERFORM 9900-MOVER-ERRO-SQL

               MOVE SPACES             TO WAFG-DADOS-RST

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2841-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9900-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO WAFG-ERRO.
           MOVE 'GFCT0142'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          WRK-SQLCODE.

           MOVE WRK-COD-SQL            TO WAFG-COD-SQL-ERRO

           MOVE SPACES                 TO WAFG-DESC-MSG-ERRO.

           PERFORM 9990-OBTER-DESC-MENSAGEM.

           STRING GFCTG3-DESC-MSG
                  ' - '
                  WRK-COD-TABELA
           DELIMITED BY '  '           INTO WAFG-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9990-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAFF-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAFF-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAFF-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAFG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG001-MODULO
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0142'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO WAFG-ERRO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0142'     TO GFCT0M-TRANSACAO
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
