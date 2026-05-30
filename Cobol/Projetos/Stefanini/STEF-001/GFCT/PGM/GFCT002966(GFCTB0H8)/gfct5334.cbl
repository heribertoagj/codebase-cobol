      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT5334.
       AUTHOR.     RAFAEL TESTA.
      *================================================================*
      *                         S O N D A - I T                        *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT5334                                     *
      *    PROGRAMADOR.:  RAFAEL TESTA                - SONDA IT       *
      *    ANALISTA....:  RAFAEL TESTA                - SONDA IT       *
      *    DATA........:  09/03/2012                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PESQUISA DAS CESTAS POR SEGMENTO             *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTIR  - AREA DE ENTRADA DO MODULO                       *
      *    I#GFCTIS  - AREA DE SAIDA DO MODULO                         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                   DB2PRD.SERVC_TARIF_PRINC     GFCTB0D8        *
      *                   DB2PRD.PARM_DATA_PROCM       GFCTB0A1        *
      *                   DB2PRD.PARM_SERVC_TARIF      GFCTB0A2        *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE     GFCTB0C6        *
      *                   DB2PRD.PRMSS_GRP_SGMTO       GFCTB0D2        *
      *                   DB2PRD.TASSOC_SGMTO_CLI      GFCTB0H3        *
BI0115*----------------------------------------------------------------*
BI0115*                 U L T I M A   A L T E R A C A O                *
BI0115*----------------------------------------------------------------*
BI0115*                 SONDA IT  - ALTERACAO - BI0115                 *
BI0115*----------------------------------------------------------------*
BI0115*                                                                *
BI0115*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0115*    DATA........:  01 / 2015                                    *
BI0115*    OBJETIVO....:  TRATAR TIPO DE TARIFA = 4                    *
BI0115*                   ( PACOTE PERSONALIZADO )                     *
CI0416*----------------------------------------------------------------*
CI0416*                 U L T I M A   A L T E R A C A O                *
CI0416*----------------------------------------------------------------*
CI0416*                 SONDA IT  - ALTERACAO - BI0115                 *
CI0416*----------------------------------------------------------------*
CI0416*                                                                *
CI0416*    ANALISTA....:  CIBELE BARBOSA  - SONDA - IT                 *
CI0416*    DATA........:  04 / 2016                                    *
CI0416*    OBJETIVO....:  CRIAR A VERSAO 3 E OBTER O TIPO DE CONTA     *
CI0416*                   (GFCTB0D8) - PROGRAMA DE BENEFICIOS.         *
CI0416*----------------------------------------------------------------*
RE0516*    ANALISTA....:  REGINA          - SONDA - IT                 *
RE0516*    DATA........:  05 / 2016                                    *
RE0516*    OBJETIVO....:  FILTRAR TIPO CONTA DEBITO DEPENDENDO DO      *
RE0516*                   PARAMETRO COD VERSAO.                        *
      *================================================================*
HE1016*    ANALISTA....:  HEYDI RAFAELA MARIGO - BRADESCO              *
HE1016*    DATA........:  10 / 2016                                    *
HE1016*    OBJETIVO....:  INCLUIR O CAMPO VALOR NO LAYOUT UTILIZADO    *
HE1016*                   NA CONSULTA DE VERS„O 3.                     *
      *================================================================*
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *----------------------------------------------------------------*

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT5334  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       77 ACU-OBTIDOS-GFCTB0D8         PIC S9(007) COMP-3  VALUE ZEROS.
       77 ACU-OBTIDOS-GFCTB0A2         PIC S9(007) COMP-3  VALUE ZEROS.
       77 ACU-OBTIDOS-GFCTB0D2         PIC S9(007) COMP-3  VALUE ZEROS.
       77 ACU-OBTIDOS-GFCTB0H3         PIC S9(007) COMP-3  VALUE ZEROS.
       77 ACU-OBTIDOS-C6-09            PIC S9(007) COMP-3  VALUE ZEROS.
       77 WRK-FIM-D8                   PIC  X(001)         VALUE 'N'.
       77 WRK-FIM-C6-09                PIC  X(001)         VALUE 'N'.
       77 WRK-FIM-H3                   PIC  X(001)         VALUE 'N'.
       77 WRK-ABEND                    PIC  X(004)         VALUE SPACES.
       77 WRK-DESPR                    PIC S9(007) COMP-3  VALUE ZEROS.
       77 WRK-NRPAG                    PIC  9(007) COMP-3  VALUE ZEROS.
       77 WRK-NRAUX                    PIC S9(005) COMP-3  VALUE ZEROS.
       77 WRK-QTREG                    PIC S9(005) COMP-3  VALUE ZEROS.
       77 WRK-QTDE                     PIC S9(009) COMP-3  VALUE ZEROS.
       77 WRK-PAGINA                   PIC  9(07)  COMP-3  VALUE ZEROS.
       77 WRK-QTD-LINHA                PIC  9(05)  COMP-3  VALUE ZEROS.
       77 WRK-UTPAG                    PIC  9(007) COMP-3  VALUE ZEROS.
       77 WRK-IND                      PIC  9(007) COMP-3  VALUE ZEROS.
       01 WRK-EIB                      PIC  9(008) COMP-3  VALUE ZEROS.

       01  WRK-TARI-S                  PIC +9(005) VALUE ZEROS.
       01  FILLER REDEFINES WRK-TARI-S.
           05     FILLER               PIC X(001).
           05     WRK-TARI-R           PIC 9(005).

       01  WRK-SQL-COM-S               PIC S9(009) VALUE ZEROS.
       01  FILLER REDEFINES WRK-SQL-COM-S.
           05     FILLER               PIC X(01).
           05     WRK-SQL-COM-SS       PIC 9(009).

       01 WRK-QTREG1                   PIC +9(005) VALUE ZEROS.
       01 FILLER REDEFINES WRK-QTREG1.
          05 FILLER                    PIC X.
          05 WRK-QTREG1-R              PIC 9(5).

       01  WRK-SQL-COM                 PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-DATA-DMAB.
           10 WRK-DATA-DMAB-D          PIC  9(02) VALUE ZERO.
           10 FILLER                   PIC  X(01) VALUE SPACES.
           10 WRK-DATA-DMAB-M          PIC  9(02) VALUE ZERO.
           10 FILLER                   PIC  X(01) VALUE SPACES.
           10 WRK-DATA-DMAB-A          PIC  9(04) VALUE ZERO.

       01  WRK-DATA-DMA.
           10 WRK-DATA-DMA-D          PIC  9(02) VALUE ZERO.
           10 WRK-DATA-DMA-M          PIC  9(02) VALUE ZERO.
           10 WRK-DATA-DMA-A          PIC  9(04) VALUE ZERO.
       01  WRK-DATA-DMA-R REDEFINES WRK-DATA-DMA PIC 9(8).



       01  WRK-CSGMTO-INIC-CLI         PIC +9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CSGMTO-INIC-CLI.
           05  FILLER                  PIC  X(001).
           05  WRK-CSGMTO-INIC         PIC  9(003).

       01  WRK-CSGMTO-FNAL-CLI         PIC +9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CSGMTO-FNAL-CLI.
           05  FILLER                  PIC  X(001).
           05  WRK-CSGMTO-FNAL         PIC  9(003).

       01  WRK-SQL                     PIC  9(009)         VALUE ZEROS.
       01  WRK-SQL-R   REDEFINES  WRK-SQL.
           05   FILLER                 PIC  9(005).
           05   WRK-PARTE2             PIC  9(004).

       01  WRK-TP-PESSOA               PIC  X(001)         VALUE ZEROS.

BI0115 01  WRK-CTPO-SERVC-TARIF-2      PIC S9(001) COMP-3   VALUE +2.
BI0115 01  WRK-CTPO-SERVC-TARIF-4      PIC S9(001) COMP-3   VALUE +4.

RE0516 01  WRK-CTPO-CTA-DEB-TARIF-A    PIC S9(001) COMP-3   VALUE +0.
RE0516 01  WRK-CTPO-CTA-DEB-TARIF-B    PIC S9(001) COMP-3   VALUE +0.

       01  IND-1                       PIC S9(009) COMP-3  VALUE ZEROS.

       01  WRK-IND-1                   PIC +9(9) VALUE ZEROS.
       01  FILLER REDEFINES WRK-IND-1.
           05 FILLER PIC X.
           05 FILLER.
              10 FILLER PIC X(2).
              10 WRK-IND-1-7   PIC 9(7).

       01  WRK-AREA-GFCTWKK1.
       COPY 'GFCTWKK1'.


       01  FILLER.
           03 WRK-AREA-GFCTWAT6.
           COPY 'GFCTWAT6'.

CI0416 01  FILLER.
CI0416     03 WRK-AREA-GFCTWBE1.
CI0416     COPY 'GFCTWBE1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     TRATAMENTO DE ERRO       *'.
      *----------------------------------------------------------------*

       COPY 'GFCTW0ER'.

       COPY 'GFCTW000'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         AREA PARA DB2        *'.
      *----------------------------------------------------------------*

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
             INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H3
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0C6 CURSOR FOR
             SELECT CSERVC_TARIF
                   ,CAGPTO_CTA
                   ,CSEQ_AGPTO_CTA
                   ,DINIC_PRMSS_PCOTE
                   ,CFUNC_MANUT_INCL
                   ,CFUNC_MANUT
                   ,DFIM_PRMSS_PCOTE
                   ,CINDCD_ADSAO_INDVD
                   ,CINDCD_AGPTO_TOT
                   ,HMANUT_REG
                   ,HINCL_REG_SIST
                   ,CINDCD_EXCEC_ADSAO
                   ,CIDTFD_PRMSS_INDVD
                   ,CPSSOA_SERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE CSERVC_TARIF        = :GFCTB0C6.CSERVC-TARIF
             AND   CAGPTO_CTA          = :GFCTB0C6.CAGPTO-CTA
             AND   (CINDCD_ADSAO_INDVD  = 'I' OR
                    CINDCD_ADSAO_INDVD  = 'A')
             AND   DINIC_PRMSS_PCOTE  <= :GFCTB0C6.DINIC-PRMSS-PCOTE
             AND   DFIM_PRMSS_PCOTE   >= :GFCTB0C6.DFIM-PRMSS-PCOTE
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB0D8 CURSOR FOR
             SELECT
                   CSERVC_TARIF,
CI0416             CTPO_CTA_DEB_TARIF,
                   CPSSOA_SERVC_TARIF,
                   RSERVC_TARIF_REDZD
             FROM  DB2PRD.SERVC_TARIF_PRINC
             WHERE
BI0115              CSERVC_TARIF      >= :GFCTB0D8.CSERVC-TARIF
BI0115       AND   (CTPO_SERVC_TARIF   = :WRK-CTPO-SERVC-TARIF-2
BI0115         OR   CTPO_SERVC_TARIF   = :WRK-CTPO-SERVC-TARIF-4)
BI0115***__________CTPO_SERVC_TARIF____=_:GFCTB0D8.CTPO-SERVC-TARIF
RE0516       AND   (CTPO_CTA_DEB_TARIF  = :WRK-CTPO-CTA-DEB-TARIF-A
RE0516         OR  CTPO_CTA_DEB_TARIF   = :WRK-CTPO-CTA-DEB-TARIF-B)
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR03-GFCTB0H3 CURSOR FOR
             SELECT
                   CSGMTO_INIC_CLI,
                   CSGMTO_FNAL_CLI
             FROM  DB2PRD.TASSOC_SGMTO_CLI
             WHERE
                   CSGMTO_GSTAO_TARIF  = :GFCTB0H3.CSGMTO-GSTAO-TARIF
             AND   DINIC_VGCIA_ASSOC  <= :GFCTB0H3.DINIC-VGCIA-ASSOC
             AND   DFIM_VGCIA_ASSOC   >= :GFCTB0H3.DFIM-VGCIA-ASSOC
             ORDER BY CSGMTO_GSTAO_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT5334   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
         03 LNK-AREA-GFCTWAT3.
            COPY 'GFCTWAT3'.

      *================================================================*
       PROCEDURE                      DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                HANDLE ABEND
                LABEL  (9990-TRATAR-ERRO-CICS)
           END-EXEC.

           PERFORM 1000-INICIALIZAR-PROGRAMA

           PERFORM 2100-OBTER-DADOS-GFCTB0A1

           PERFORM 1500-PROCESSAR

           IF  WRK-FIM-D8             EQUAL 'N'
               MOVE IND-1             TO  WRK-IND-1
               MOVE WRK-IND-1-7       TO  GFCTWAT3-QTD-REGISTRO
               MOVE 1                 TO  GFCTWAT3-COD-RETORNO
               MOVE 'A'               TO  GFCTWAT3-NIVEL-MSGEM
               MOVE 'N'               TO  GFCTWAT3-FIM
           ELSE
               MOVE IND-1             TO  WRK-IND-1
               MOVE WRK-IND-1-7       TO  GFCTWAT3-QTD-REGISTRO
               MOVE 0                 TO  GFCTWAT3-COD-RETORNO
               MOVE 'S'               TO  GFCTWAT3-NIVEL-MSGEM
               MOVE 'S'               TO  GFCTWAT3-FIM
           END-IF

           IF  IND-1                  GREATER ZEROS
               MOVE 'CONSULTA EFETUADA COM SUCESSO'
                                      TO  GFCTWAT3-RMSG-INFOR
           ELSE
               MOVE 'NAO EXISTEM DADOS PARA O SEGMENTO INFORMADO'
                                      TO  GFCTWAT3-RMSG-INFOR
           END-IF

           PERFORM 9992-FINALIZAR-PROCESSO
           .
      *----------------------------------------------------------------*
       0000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR-PROGRAMA      SECTION.
      *----------------------------------------------------------------*

           MOVE DFHCOMMAREA(1:EIBCALEN)
                                 TO  LNK-AREA-GFCTWAT3


           IF  GFCTWAT3-SISTEMA  NOT EQUAL 'GFCT'
           OR  GFCTWAT3-SISTEMA      EQUAL SPACES
               MOVE 'GFCT'       TO  GFCTER-SISTEMA
               MOVE 1079         TO  GFCTER-NSEQ-MSGEM
               MOVE 0            TO  GFCTER-COD-RETORNO
               MOVE 2            TO  GFCTWAT3-COD-RETORNO
               MOVE 'B'          TO  GFCTWAT3-NIVEL-MSGEM
                                     GFCTER-CTPO-MSGEM
               PERFORM 9991-FORMATAR-MENSAGEM-ERRO
               PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           IF  GFCTWAT3-QTD-LINHA
                                 EQUAL ZEROS
               MOVE 'GFCT'       TO  GFCTER-SISTEMA
               MOVE 1068         TO  GFCTER-NSEQ-MSGEM
               MOVE 0            TO  GFCTER-COD-RETORNO
               MOVE 2            TO  GFCTWAT3-COD-RETORNO
               MOVE 'B'          TO  GFCTWAT3-NIVEL-MSGEM
                                     GFCTER-CTPO-MSGEM
               PERFORM 9991-FORMATAR-MENSAGEM-ERRO
               PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           MOVE GFCTWAT3-QTD-LINHA TO WRK-QTD-LINHA.

           IF  GFCTWAT3-PAGINA   EQUAL ZEROS
               MOVE 'GFCT'       TO  GFCTER-SISTEMA
               MOVE 1603         TO  GFCTER-NSEQ-MSGEM
               MOVE 0            TO  GFCTER-COD-RETORNO
               MOVE 2            TO  GFCTWAT3-COD-RETORNO
               MOVE 'B'          TO  GFCTWAT3-NIVEL-MSGEM
                                     GFCTER-CTPO-MSGEM
               PERFORM 9991-FORMATAR-MENSAGEM-ERRO
               PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           MOVE GFCTWAT3-PAGINA  TO WRK-PAGINA.

           IF  GFCTWAT3-QTD-REGISTRO
                                 NOT NUMERIC
               MOVE 'GFCT'       TO  GFCTER-SISTEMA
               MOVE 1068         TO  GFCTER-NSEQ-MSGEM
               MOVE 0            TO  GFCTER-COD-RETORNO
               MOVE 2            TO  GFCTWAT3-COD-RETORNO
               MOVE 'B'          TO  GFCTWAT3-NIVEL-MSGEM
                                     GFCTER-CTPO-MSGEM
               PERFORM 9991-FORMATAR-MENSAGEM-ERRO
               PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           MOVE 99999              TO WRK-QTREG

CI0416     IF  GFCTWAT3-E-COD-VERSAO EQUAL 001 OR 002 OR 003
               IF GFCTWAT3-E-COD-VERSAO EQUAL 001
HE1016            MOVE 200 TO WRK-QTDE
               ELSE
                  IF GFCTWAT3-E-COD-VERSAO EQUAL 002
                     MOVE 169 TO WRK-QTDE
                  ELSE
HE1016               MOVE 169 TO WRK-QTDE
                  END-IF
               END-IF
           ELSE
               MOVE 'GFCT'       TO  GFCTER-SISTEMA
               MOVE 59           TO  GFCTER-NSEQ-MSGEM
               MOVE 0            TO  GFCTER-COD-RETORNO
               MOVE 2            TO  GFCTWAT3-COD-RETORNO
               MOVE 'O'          TO  GFCTWAT3-NIVEL-MSGEM
                                     GFCTER-CTPO-MSGEM
               PERFORM 9991-FORMATAR-MENSAGEM-ERRO
               PERFORM 9992-FINALIZAR-PROCESSO
           END-IF.

           IF  GFCTWAT3-E-COD-SEGMENTO
                                 NOT NUMERIC
               MOVE 'GFCT'       TO  GFCTER-SISTEMA
               MOVE 500          TO  GFCTER-NSEQ-MSGEM
               MOVE 0            TO  GFCTER-COD-RETORNO
               MOVE 2            TO  GFCTWAT3-COD-RETORNO
               MOVE 'B'          TO  GFCTWAT3-NIVEL-MSGEM
                                     GFCTER-CTPO-MSGEM
               PERFORM 9991-FORMATAR-MENSAGEM-ERRO
               PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           IF  (GFCTWAT3-E-TP-PESSOA
                                 EQUAL SPACES OR LOW-VALUES) OR
               (GFCTWAT3-E-TP-PESSOA NOT EQUAL 'F' AND 'J')
               MOVE 'GFCT'       TO  GFCTER-SISTEMA
               MOVE 461          TO  GFCTER-NSEQ-MSGEM
               MOVE 0            TO  GFCTER-COD-RETORNO
               MOVE 2            TO  GFCTWAT3-COD-RETORNO
               MOVE 'B'          TO  GFCTWAT3-NIVEL-MSGEM
                                     GFCTER-CTPO-MSGEM
               PERFORM 9991-FORMATAR-MENSAGEM-ERRO
               PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           EVALUATE   GFCTWAT3-E-TP-PESSOA
               WHEN   'F'
                   MOVE '1'   TO WRK-TP-PESSOA
               WHEN   'J'
                   MOVE '2'   TO WRK-TP-PESSOA
           END-EVALUATE.

           IF  GFCTWAT3-E-TP-PESQUISA
                                 NOT NUMERIC
               MOVE 'GFCT'       TO  GFCTER-SISTEMA
               MOVE 138          TO  GFCTER-NSEQ-MSGEM
               MOVE 0            TO  GFCTER-COD-RETORNO
               MOVE 2            TO  GFCTWAT3-COD-RETORNO
               MOVE 'B'          TO  GFCTWAT3-NIVEL-MSGEM
                                     GFCTER-CTPO-MSGEM
               PERFORM 9991-FORMATAR-MENSAGEM-ERRO
               PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .
      *----------------------------------------------------------------*
       1000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 7000-TRATAR-PAG

           PERFORM 1510-ABRIR-CURSOR

           PERFORM 1520-LER-CURSOR
             UNTIL WRK-FIM-D8         EQUAL 'S'
                OR ACU-OBTIDOS-GFCTB0D8
                                      GREATER WRK-DESPR

           PERFORM 1530-PROCESSAR-CURSOR
             UNTIL WRK-FIM-D8         EQUAL 'S'
                OR IND-1              EQUAL WRK-QTDE

           PERFORM 1540-FECHAR-CURSOR
           .
      *----------------------------------------------------------------*
       1500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1510-ABRIR-CURSOR          SECTION.
      *----------------------------------------------------------------*

BI0115***  MOVE_2____________________TO_CTPO-SERVC-TARIF____OF_GFCTB0D8

BI0115     MOVE ZEROS                TO CSERVC-TARIF        OF GFCTB0D8

BI0115     MOVE +2                   TO WRK-CTPO-SERVC-TARIF-2
BI0115     MOVE +4                   TO WRK-CTPO-SERVC-TARIF-4

RE0516     IF  GFCTWAT3-E-COD-VERSAO EQUAL 001 OR 002
RE0516         MOVE 1                TO WRK-CTPO-CTA-DEB-TARIF-A
RE0516                                  WRK-CTPO-CTA-DEB-TARIF-B
RE0516     ELSE
RE0516         MOVE 1                TO WRK-CTPO-CTA-DEB-TARIF-A
RE0516         MOVE 2                TO WRK-CTPO-CTA-DEB-TARIF-B
RE0516     END-IF.

           EXEC SQL
                OPEN CSR02-GFCTB0D8
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS ) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0D8'         TO GFCT00-TAB-DB2
                MOVE 'OPEN'             TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 1                  TO GFCT00-LOC-DB2
                MOVE 'ERRO ABERTURA'    TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .
      *----------------------------------------------------------------*
       1510-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1520-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR02-GFCTB0D8 INTO
                   :GFCTB0D8.CSERVC-TARIF,
CI0416             :GFCTB0D8.CTPO-CTA-DEB-TARIF,
                   :GFCTB0D8.CPSSOA-SERVC-TARIF,
                   :GFCTB0D8.RSERVC-TARIF-REDZD
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0D8'         TO GFCT00-TAB-DB2
                MOVE 'FETCH'            TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 2                  TO GFCT00-LOC-DB2
                MOVE 'ERRO LEITURA'     TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           IF  SQLCODE                 EQUAL ZEROS
               ADD 1                   TO ACU-OBTIDOS-GFCTB0D8
           ELSE
               MOVE 'S'                TO WRK-FIM-D8
           END-IF
           .
      *----------------------------------------------------------------*
       1520-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1530-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-OBTER-DADOS-GFCTB0A2

           PERFORM 1520-LER-CURSOR
           .
      *----------------------------------------------------------------*
       1530-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1540-FECHAR-CURSOR         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR02-GFCTB0D8
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0D8'         TO GFCT00-TAB-DB2
                MOVE 'CLOSE'            TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 3                  TO GFCT00-LOC-DB2
                MOVE 'ERRO FECHAMENTO'  TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .
      *----------------------------------------------------------------*
       1540-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-OBTER-DADOS-GFCTB0A1  SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0A1'         TO GFCT00-TAB-DB2
                MOVE 'SELECT'           TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 4                  TO GFCT00-LOC-DB2
                MOVE 'ERRO SELECT'      TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .

           MOVE DPROCM-ATUAL OF GFCTB0A1 TO WRK-DATA-DMAB.
           MOVE WRK-DATA-DMAB-D TO WRK-DATA-DMA-D.
           MOVE WRK-DATA-DMAB-M TO WRK-DATA-DMA-M.
           MOVE WRK-DATA-DMAB-A TO WRK-DATA-DMA-A.

      *----------------------------------------------------------------*
       2100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-OBTER-DADOS-GFCTB0A2  SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF   OF GFCTB0D8
                               TO CSERVC-TARIF        OF GFCTB0A2

           MOVE DPROCM-ATUAL   OF GFCTB0A1
                               TO DINIC-VGCIA-TARIF   OF GFCTB0A2

           MOVE DPROCM-ATUAL   OF GFCTB0A1
                               TO DFIM-VGCIA-TARIF    OF GFCTB0A2

           EXEC SQL
             SELECT
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF,
                   RSEGDA_LIN_EXTRT
             INTO
                   :GFCTB0A2.DINIC-VGCIA-TARIF,
                   :GFCTB0A2.DFIM-VGCIA-TARIF,
                   :GFCTB0A2.RSEGDA-LIN-EXTRT
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
             AND   DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF
             AND   DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC

           IF  (SQLCODE              NOT EQUAL ZEROS AND +100 AND -811)
           OR  (SQLWARN0             EQUAL 'W')
                MOVE 'GFCT5334'      TO GFCT00-PGM
                MOVE 'GFCTB0A2'      TO GFCT00-TAB-DB2
                MOVE 'SELECT'        TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2      TO GFCT00-SQLCODE
                MOVE 5               TO GFCT00-LOC-DB2
                MOVE 'ERRO SELECT'   TO GFCT00-MSG-DB2
                MOVE 4               TO GFCTER-COD-RETORNO
                MOVE 'C'             TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           IF  SQLCODE                 EQUAL ZEROS
               ADD 1 TO ACU-OBTIDOS-GFCTB0A2
               MOVE SPACES TO WRK-FIM-C6-09
               PERFORM 2300-OBTER-DADOS-CAGPTO09
               PERFORM 2600-OBTER-DADOS-CAGPTO14
           END-IF
           .
      *----------------------------------------------------------------*
       2200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-OBTER-DADOS-CAGPTO09  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-ABRIR-CURSOR

           MOVE 'N' TO WRK-FIM-C6-09

           PERFORM 2320-LER-CURSOR

           PERFORM 2330-PROCESSAR-CURSOR
             UNTIL WRK-FIM-C6-09       EQUAL 'S'
                OR IND-1               EQUAL WRK-QTDE

           PERFORM 2340-FECHAR-CURSOR
           .
      *----------------------------------------------------------------*
       2300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-ABRIR-CURSOR          SECTION.
      *----------------------------------------------------------------*

           MOVE 9                 TO CAGPTO-CTA         OF GFCTB0C6
______*____MOVE_'I'_______________TO_CINDCD-ADSAO-INDVD_OF_GFCTB0C6
           MOVE CSERVC-TARIF      OF GFCTB0D8
                                  TO CSERVC-TARIF       OF GFCTB0C6
           MOVE DPROCM-ATUAL      OF GFCTB0A1
                                  TO DINIC-PRMSS-PCOTE  OF GFCTB0C6
           MOVE DPROCM-ATUAL      OF GFCTB0A1
                                  TO DFIM-PRMSS-PCOTE   OF GFCTB0C6

           EXEC SQL
                OPEN CSR01-GFCTB0C6
           END-EXEC

           IF  (SQLCODE                 NOT EQUAL ZEROS) OR
               (SQLWARN0                EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0C6'         TO GFCT00-TAB-DB2
                MOVE 'OPEN'             TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 6                  TO GFCT00-LOC-DB2
                MOVE 'ERRO ABERTURA'    TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .
      *----------------------------------------------------------------*
       2310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2320-LER-CURSOR            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0C6 INTO
                   :GFCTB0C6.CSERVC-TARIF
                  ,:GFCTB0C6.CAGPTO-CTA
                  ,:GFCTB0C6.CSEQ-AGPTO-CTA
                  ,:GFCTB0C6.DINIC-PRMSS-PCOTE
                  ,:GFCTB0C6.CFUNC-MANUT-INCL
                  ,:GFCTB0C6.CFUNC-MANUT
                  ,:GFCTB0C6.DFIM-PRMSS-PCOTE
                  ,:GFCTB0C6.CINDCD-ADSAO-INDVD
                  ,:GFCTB0C6.CINDCD-AGPTO-TOT
                  ,:GFCTB0C6.HMANUT-REG
                  ,:GFCTB0C6.HINCL-REG-SIST
                  ,:GFCTB0C6.CINDCD-EXCEC-ADSAO
                  ,:GFCTB0C6.CIDTFD-PRMSS-INDVD
                  ,:GFCTB0C6.CPSSOA-SERVC-TARIF
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0C6'         TO GFCT00-TAB-DB2
                MOVE 'FETCH'            TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 7                  TO GFCT00-LOC-DB2
                MOVE 'ERRO LEITURA'     TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           IF  SQLCODE                 EQUAL ZEROS
               ADD 1 TO ACU-OBTIDOS-C6-09
           ELSE
               MOVE 'S'                TO WRK-FIM-C6-09
           END-IF
           .
      *----------------------------------------------------------------*
       2320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2330-PROCESSAR-CURSOR      SECTION.
      *----------------------------------------------------------------*

           IF  CPSSOA-SERVC-TARIF      OF GFCTB0D8
                                       EQUAL WRK-TP-PESSOA
               PERFORM 2400-OBTER-DADOS-GFCTB0D2
           ELSE
               IF (CPSSOA-SERVC-TARIF  OF GFCTB0D8
                                       NOT EQUAL WRK-TP-PESSOA) AND
                  (CPSSOA-SERVC-TARIF  OF GFCTB0D8
                                       NOT EQUAL '1' AND '2')
                   PERFORM 2400-OBTER-DADOS-GFCTB0D2
               END-IF
           END-IF

           PERFORM 2320-LER-CURSOR
           .
      *----------------------------------------------------------------*
       2330-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2340-FECHAR-CURSOR         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0C6
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0C6'         TO GFCT00-TAB-DB2
                MOVE 'CLOSE'            TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 8                  TO GFCT00-LOC-DB2
                MOVE 'ERRO FECHAMENTO'  TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .
      *----------------------------------------------------------------*
       2340-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-OBTER-DADOS-GFCTB0D2  SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF      OF GFCTB0D8
                                  TO CSERVC-TARIF      OF GFCTB0D2

           MOVE CAGPTO-CTA        OF GFCTB0C6
                                  TO CAGPTO-CTA        OF GFCTB0D2

           MOVE CSEQ-AGPTO-CTA    OF GFCTB0C6
                                  TO CSEQ-AGPTO-CTA    OF GFCTB0D2

           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6
                                  TO DINIC-PRMSS-PCOTE OF GFCTB0D2

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0D2.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.PRMSS_GRP_SGMTO
             WHERE
                   CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
             AND   CSEQ_AGPTO_CTA     = :GFCTB0D2.CSEQ-AGPTO-CTA
             AND   DINIC_PRMSS_PCOTE  = :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0D2'         TO GFCT00-TAB-DB2
                MOVE 'SELECT'           TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 9                  TO GFCT00-LOC-DB2
                MOVE 'ERRO SELECT'      TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           IF  SQLCODE                 EQUAL ZEROS
               ADD 1 TO ACU-OBTIDOS-GFCTB0D2
               PERFORM 2500-OBTER-DADOS-GFCTB0H3
           END-IF
           .
      *----------------------------------------------------------------*
       2400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-OBTER-DADOS-GFCTB0H3  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2510-ABRIR-CURSOR

           MOVE 'N' TO WRK-FIM-H3

           PERFORM 2520-LER-CURSOR

           PERFORM 2530-PROCESSAR-CURSOR
             UNTIL WRK-FIM-H3          EQUAL 'S'
                OR IND-1               EQUAL WRK-QTDE

           PERFORM 2540-FECHAR-CURSOR
           .
      *----------------------------------------------------------------*
       2500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-ABRIR-CURSOR          SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0D2
                                   TO CSGMTO-GSTAO-TARIF  OF GFCTB0H3

           MOVE DPROCM-ATUAL       OF GFCTB0A1
                                   TO DINIC-VGCIA-ASSOC   OF GFCTB0H3

           MOVE DPROCM-ATUAL       OF GFCTB0A1
                                   TO DFIM-VGCIA-ASSOC    OF GFCTB0H3

           EXEC SQL
                OPEN CSR03-GFCTB0H3
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0H3'         TO GFCT00-TAB-DB2
                MOVE 'OPEN'             TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 10                 TO GFCT00-LOC-DB2
                MOVE 'ERRO ABERTURA'    TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .
      *----------------------------------------------------------------*
       2510-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2520-LER-CURSOR            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR03-GFCTB0H3 INTO
                   :GFCTB0H3.CSGMTO-INIC-CLI,
                   :GFCTB0H3.CSGMTO-FNAL-CLI
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0H3'         TO GFCT00-TAB-DB2
                MOVE 'FETCH'            TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 11                 TO GFCT00-LOC-DB2
                MOVE 'ERRO LEITURA'     TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           IF  SQLCODE                 EQUAL ZEROS
               ADD 1 TO ACU-OBTIDOS-GFCTB0H3
           ELSE
               MOVE 'S'                TO WRK-FIM-H3
               GO                      TO 2520-99-FIM
           END-IF
           .
      *----------------------------------------------------------------*
       2520-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2530-PROCESSAR-CURSOR      SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-INIC-CLI        OF GFCTB0H3
                                       TO WRK-CSGMTO-INIC-CLI

           MOVE CSGMTO-FNAL-CLI        OF GFCTB0H3
                                       TO WRK-CSGMTO-FNAL-CLI

           IF  GFCTWAT3-E-TP-PESQUISA  EQUAL ZEROS
               IF  WRK-CSGMTO-INIC     NOT GREATER
                                          GFCTWAT3-E-COD-SEGMENTO
               AND WRK-CSGMTO-FNAL     NOT LESS
                                          GFCTWAT3-E-COD-SEGMENTO
                   PERFORM 2700-ACESSAR-GFCTB0A2
                   PERFORM 3500-GRAVAR-SAIDA
               END-IF
           END-IF.

           IF  GFCTWAT3-E-TP-PESQUISA  EQUAL 1
               IF  GFCTWAT3-E-COD-SEGMENTO
                                      EQUAL WRK-CSGMTO-INIC
               AND GFCTWAT3-E-COD-SEGMENTO
                                      EQUAL WRK-CSGMTO-FNAL
                   PERFORM 2700-ACESSAR-GFCTB0A2
                   PERFORM 3500-GRAVAR-SAIDA
               END-IF
           END-IF.

           PERFORM 2520-LER-CURSOR.
           .
      *----------------------------------------------------------------*
       2530-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2540-FECHAR-CURSOR         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR03-GFCTB0H3
           END-EXEC

           IF  (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0H3'         TO GFCT00-TAB-DB2
                MOVE 'CLOSE'            TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 12                 TO GFCT00-LOC-DB2
                MOVE 'ERRO FECHAMENTO'  TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .
      *----------------------------------------------------------------*
       2540-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-OBTER-DADOS-CAGPTO14  SECTION.
      *----------------------------------------------------------------*

           MOVE 14                TO CAGPTO-CTA         OF GFCTB0C6
______*____MOVE_'I'_______________TO_CINDCD-ADSAO-INDVD_OF_GFCTB0C6
           MOVE 'S'               TO CINDCD-AGPTO-TOT   OF GFCTB0C6
           MOVE CSERVC-TARIF      OF GFCTB0D8
                                  TO CSERVC-TARIF       OF GFCTB0C6
           MOVE DPROCM-ATUAL      OF GFCTB0A1
                                  TO DINIC-PRMSS-PCOTE  OF GFCTB0C6
           MOVE DPROCM-ATUAL      OF GFCTB0A1
                                  TO DFIM-PRMSS-PCOTE   OF GFCTB0C6

           EXEC SQL
             SELECT CSERVC_TARIF,
                    CAGPTO_CTA,
                    CSEQ_AGPTO_CTA,
                    DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
                   :GFCTB0C6.CAGPTO-CTA,
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA
             AND   (CINDCD_ADSAO_INDVD = 'I' OR
                    CINDCD_ADSAO_INDVD = 'A')
             AND   CINDCD_AGPTO_TOT   = :GFCTB0C6.CINDCD-AGPTO-TOT
             AND   DINIC_PRMSS_PCOTE <= :GFCTB0C6.DINIC-PRMSS-PCOTE
             AND   DFIM_PRMSS_PCOTE  >= :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC

           IF  (SQLCODE             NOT EQUAL ZEROS AND +100 AND -811)
           OR  (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0C6'         TO GFCT00-TAB-DB2
                MOVE 'SELECT'           TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 13                 TO GFCT00-LOC-DB2
                MOVE 'ERRO SELECT'      TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2700-ACESSAR-GFCTB0A2
           END-IF
           .
      *----------------------------------------------------------------*
       2600-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-ACESSAR-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF   OF GFCTB0D8
                               TO CSERVC-TARIF        OF GFCTB0A2
           MOVE DPROCM-ATUAL   OF GFCTB0A1
                               TO DINIC-VGCIA-TARIF   OF GFCTB0A2
           MOVE DPROCM-ATUAL   OF GFCTB0A1
                               TO DFIM-VGCIA-TARIF    OF GFCTB0A2


           EXEC SQL
                SELECT
                     CINDCD_CREDT_CLULR
                INTO
                    :GFCTB0A2.CINDCD-CREDT-CLULR
                FROM   DB2PRD.PARM_SERVC_TARIF
                WHERE  CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
                  AND  DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF
                  AND  DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC

           IF  (SQLCODE            NOT EQUAL ZEROS AND +100 AND -811) OR
               (SQLWARN0                 EQUAL 'W')
                MOVE 'GFCT5334'         TO GFCT00-PGM
                MOVE 'GFCTB0A2'         TO GFCT00-TAB-DB2
                MOVE 'SELECT'           TO GFCT00-FUN-DB2
                MOVE SQLCODE            TO WRK-SQL-COM-S
                MOVE WRK-SQL-COM-SS     TO WRK-SQL-COM
                MOVE WRK-SQL-COM        TO WRK-SQL
                MOVE WRK-PARTE2         TO GFCT00-SQLCODE
                MOVE 14                 TO GFCT00-LOC-DB2
                MOVE 'ERRO SELECT'      TO GFCT00-MSG-DB2
                MOVE 4                  TO GFCTER-COD-RETORNO
                MOVE 'C'                TO GFCTER-CTPO-MSGEM
                MOVE GFCT00-ERRO-DB2(1:70)    TO GFCTER-RMSG-INFOR
                MOVE GFCTER-RMSG-INFOR  TO GFCTWAT3-RMSG-INFOR
                PERFORM 9992-FINALIZAR-PROCESSO
           END-IF
           .
      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-GRAVAR-SAIDA              SECTION.
      *----------------------------------------------------------------*

CI0416     IF  GFCTWAT3-E-COD-VERSAO EQUAL 001 OR 002
CI0416         PERFORM 3510-GRAVAR-SAIDA-1
CI0416     ELSE
CI0416         PERFORM 3520-GRAVAR-SAIDA-3
CI0416     END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3510-GRAVAR-SAIDA-1            SECTION.
      *----------------------------------------------------------------*

           ADD 1                      TO IND-1

           IF CPSSOA-SERVC-TARIF OF GFCTB0D8 EQUAL '1'
              MOVE 'F'                   TO  GFCTWAT3-S-TP-PSSOA(IND-1)
           ELSE
              IF CPSSOA-SERVC-TARIF OF GFCTB0D8 EQUAL '2'
                 MOVE 'J'                TO  GFCTWAT3-S-TP-PSSOA(IND-1)
              ELSE
                 MOVE 'A'                TO  GFCTWAT3-S-TP-PSSOA(IND-1)
              END-IF
           END-IF

           MOVE CSERVC-TARIF             OF  GFCTB0D8 TO
           WRK-TARI-S

           MOVE WRK-TARI-R
                                         TO  GFCTWAT3-S-CESTA-TARIFA
                                             (IND-1)
           MOVE RSERVC-TARIF-REDZD       OF  GFCTB0D8(1:30)
                                         TO  GFCTWAT3-S-DESC-TARIFA
                                             (IND-1)
081116******OVE RSEGDA-LIN-EXTRT         *F  GFCTB0A2(1:30)
           MOVE RSERVC-TARIF-REDZD       OF  GFCTB0D8(1:30)
                                         TO  GFCTWAT3-S-2A-LIN-CESTA
                                             (IND-1)
           MOVE CSGMTO-INIC-CLI          OF  GFCTB0H3
                                         TO  GFCTWAT3-S-SEG-INICIAL
                                             (IND-1)
           MOVE CSGMTO-FNAL-CLI          OF  GFCTB0H3
                                         TO  GFCTWAT3-S-SEG-FINAL
                                             (IND-1)

           IF CINDCD-CREDT-CLULR       OF  GFCTB0A2 EQUAL 'S'
              MOVE 'S'
                                         TO  GFCTWAT3-S-CREDT-CLULR
                                             (IND-1)
           ELSE
              MOVE 'N'
                                         TO  GFCTWAT3-S-CREDT-CLULR
                                             (IND-1)
           END-IF
           .
      *----------------------------------------------------------------*
       3510-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
CI0416 3520-GRAVAR-SAIDA-3            SECTION.
      *----------------------------------------------------------------*

           ADD 1                      TO IND-1

           MOVE GFCTWAT3-TRANSACAO      TO GFCTWBE1-TRANSACAO.
           MOVE GFCTWAT3-SISTEMA        TO GFCTWBE1-SISTEMA.
           MOVE GFCTWAT3-QTD-LINHA      TO GFCTWBE1-QTD-LINHA.
           MOVE GFCTWAT3-PAGINA         TO GFCTWBE1-PAGINA.
           MOVE GFCTWAT3-QTD-REGISTRO   TO GFCTWBE1-QTD-REGISTRO.
           MOVE GFCTWAT3-FIM            TO GFCTWBE1-FIM.
           MOVE GFCTWAT3-COD-RETORNO    TO GFCTWBE1-COD-RETORNO.
           MOVE GFCTWAT3-NIVEL-MSGEM    TO GFCTWBE1-NIVEL-MSGEM.
           MOVE GFCTWAT3-RMSG-INFOR     TO GFCTWBE1-RMSG-INFOR.
           MOVE GFCTWAT3-E-COD-VERSAO   TO GFCTWBE1-E-COD-VERSAO.
           MOVE GFCTWAT3-E-COD-SEGMENTO TO GFCTWBE1-E-COD-SEGMENTO.
           MOVE GFCTWAT3-E-TP-PESSOA    TO GFCTWBE1-E-TP-PESSOA.
           MOVE GFCTWAT3-E-TP-PESQUISA  TO GFCTWBE1-E-TP-PESQUISA.

           IF CPSSOA-SERVC-TARIF OF GFCTB0D8 EQUAL '1'
              MOVE 'F'                   TO  GFCTWBE1-S-TP-PSSOA(IND-1)
           ELSE
              IF CPSSOA-SERVC-TARIF OF GFCTB0D8 EQUAL '2'
                 MOVE 'J'                TO  GFCTWBE1-S-TP-PSSOA(IND-1)
              ELSE
                 MOVE 'A'                TO  GFCTWBE1-S-TP-PSSOA(IND-1)
              END-IF
           END-IF

           MOVE CSERVC-TARIF             OF  GFCTB0D8 TO
           WRK-TARI-S

           MOVE WRK-TARI-R
                                         TO  GFCTWBE1-S-CESTA-TARIFA
                                             (IND-1)
           MOVE RSERVC-TARIF-REDZD       OF  GFCTB0D8(1:30)
                                         TO  GFCTWBE1-S-DESC-TARIFA
                                             (IND-1)
081116******OVE RSEGDA-LIN-EXTRT         *F  GFCTB0A2(1:30)
           MOVE RSERVC-TARIF-REDZD       OF  GFCTB0D8(1:30)
                                         TO  GFCTWBE1-S-2A-LIN-CESTA
                                             (IND-1)
           MOVE CSGMTO-INIC-CLI          OF  GFCTB0H3
                                         TO  GFCTWBE1-S-SEG-INICIAL
                                             (IND-1)
           MOVE CSGMTO-FNAL-CLI          OF  GFCTB0H3
                                         TO  GFCTWBE1-S-SEG-FINAL
                                             (IND-1)
           IF CINDCD-CREDT-CLULR       OF  GFCTB0A2 EQUAL 'S'
              MOVE 'S'
                                         TO  GFCTWBE1-S-CREDT-CLULR
                                             (IND-1)
           ELSE
              MOVE 'N'
                                         TO  GFCTWBE1-S-CREDT-CLULR
                                             (IND-1)
           END-IF.

CI0416     MOVE CTPO-CTA-DEB-TARIF    OF  GFCTB0D8 TO
CI0416                                       GFCTWBE1-TIPO-CONTA-CESTA
CI0416                                       (IND-1).

HE1016     MOVE GFCTWBE1-S-CESTA-TARIFA(IND-1) TO GFCTWKK1-TARIFA.

HE1016     PERFORM 8100-OBTER-VALOR-CESTA.

HE1016     IF GFCTWKK1-COD-RETORNO NOT EQUAL ZEROS
HE1016        MOVE 99999999999,99    TO GFCTWBE1-S-VALOR-CESTA(IND-1)
HE1016     ELSE
HE1016        MOVE GFCTWKK1-VLR-BRUTO-TAR TO
HE1016                                   GFCTWBE1-S-VALOR-CESTA(IND-1)
HE1016     END-IF.

      *----------------------------------------------------------------*
       3520-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-TRATAR-PAG                SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-QTREG         TO WRK-QTREG1.
           MOVE WRK-QTREG1-R      TO GFCTWAT3-QTD-REGISTRO.

           MOVE WRK-QTREG1         TO GFCTWAT3-QTD-REGISTRO.

           COMPUTE WRK-NRPAG       = (WRK-QTREG
                                   /  WRK-QTD-LINHA).

           COMPUTE WRK-NRAUX       = (WRK-NRPAG
                                   *  WRK-QTD-LINHA).

           IF  WRK-QTREG           NOT EQUAL   WRK-NRAUX
               ADD 1               TO WRK-NRPAG
           END-IF.

           IF   WRK-PAGINA            GREATER WRK-NRPAG
           OR   WRK-PAGINA            EQUAL   WRK-UTPAG
                COMPUTE  WRK-DESPR    = ((WRK-NRPAG    - 1) *
                                      WRK-QTD-LINHA)
           ELSE
                COMPUTE  WRK-DESPR    = ((WRK-PAGINA   - 1)  *
                                      WRK-QTD-LINHA)
           END-IF.

      *----------------------------------------------------------------*
       7000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-TRATA-VERSAO-002 SECTION.
      *----------------------------------------------------------------*
           MOVE GFCTWAT3-TRANSACAO      TO GFCTWAT6-TRANSACAO.
           MOVE GFCTWAT3-SISTEMA        TO GFCTWAT6-SISTEMA.
           MOVE GFCTWAT3-QTD-LINHA      TO GFCTWAT6-QTD-LINHA.
           MOVE GFCTWAT3-PAGINA         TO GFCTWAT6-PAGINA.
           MOVE GFCTWAT3-QTD-REGISTRO   TO GFCTWAT6-QTD-REGISTRO.
           MOVE GFCTWAT3-FIM            TO GFCTWAT6-FIM.
           MOVE GFCTWAT3-COD-RETORNO    TO GFCTWAT6-COD-RETORNO.
           MOVE GFCTWAT3-NIVEL-MSGEM    TO GFCTWAT6-NIVEL-MSGEM.
           MOVE GFCTWAT3-RMSG-INFOR     TO GFCTWAT6-RMSG-INFOR.
           MOVE GFCTWAT3-E-COD-VERSAO   TO GFCTWAT6-E-COD-VERSAO.
           MOVE GFCTWAT3-E-COD-SEGMENTO TO GFCTWAT6-E-COD-SEGMENTO.
           MOVE GFCTWAT3-E-TP-PESSOA    TO GFCTWAT6-E-TP-PESSOA.
           MOVE GFCTWAT3-E-TP-PESQUISA  TO GFCTWAT6-E-TP-PESQUISA.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL
                           WRK-IND GREATER GFCTWAT3-QTD-REGISTRO
               MOVE GFCTWAT3-S-TP-PSSOA(WRK-IND)     TO
                    GFCTWAT6-S-TP-PSSOA(WRK-IND)
               MOVE GFCTWAT3-S-CESTA-TARIFA(WRK-IND) TO
                    GFCTWAT6-S-CESTA-TARIFA(WRK-IND)
               MOVE GFCTWAT3-S-DESC-TARIFA(WRK-IND)  TO
                    GFCTWAT6-S-DESC-TARIFA(WRK-IND)
               MOVE GFCTWAT3-S-2A-LIN-CESTA(WRK-IND) TO
                    GFCTWAT6-S-2A-LIN-CESTA(WRK-IND)
               MOVE GFCTWAT3-S-SEG-INICIAL(WRK-IND)  TO
                    GFCTWAT6-S-SEG-INICIAL(WRK-IND)
               MOVE GFCTWAT3-S-SEG-FINAL(WRK-IND)    TO
                    GFCTWAT6-S-SEG-FINAL(WRK-IND)
               MOVE GFCTWAT3-S-CREDT-CLULR(WRK-IND)  TO
                    GFCTWAT6-S-CREDT-CLULR(WRK-IND)
               MOVE GFCTWAT6-S-CESTA-TARIFA(WRK-IND) TO
                    GFCTWKK1-TARIFA
HE1016         PERFORM 8100-OBTER-VALOR-CESTA
               IF GFCTWKK1-COD-RETORNO NOT EQUAL ZEROS
                  MOVE 99999999999,99 TO
                  GFCTWAT6-S-VALOR-CESTA(WRK-IND)
               ELSE
                  MOVE GFCTWKK1-VLR-BRUTO-TAR TO
                  GFCTWAT6-S-VALOR-CESTA(WRK-IND)
               END-IF
           END-PERFORM.
      *----------------------------------------------------------------*
       8000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
HE1016 8100-OBTER-VALOR-CESTA         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5334'            TO GFCTWKK1-TRANSACAO.
           MOVE '0001'                TO GFCTWKK1-VERSAO.

           MOVE WRK-DATA-DMA-R        TO GFCTWKK1-DATA-REFER
                                         GFCTWKK1-DT-CONV-BRUTO
                                         GFCTWKK1-DT-CONV-EVENTO.

           MOVE 0                     TO GFCTWKK1-VALOR-EVENTO
                                         GFCTWKK1-COD-NTAX
                                         GFCTWKK1-COD-RETORNO.

           EXEC CICS
                LINK PROGRAM  ('GFCT5338')
                COMMAREA (WRK-AREA-GFCTWKK1)
                LENGTH   (LENGTH OF WRK-AREA-GFCTWKK1)
           END-EXEC.

      *----------------------------------------------------------------*
HE1016 8100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9990-TRATAR-ERRO-CICS          SECTION.
      *----------------------------------------------------------------*

           EXEC      CICS ASSIGN ABCODE (WRK-ABEND)
           END-EXEC

           MOVE 'GFCT5334'       TO GFCT00-PGM
           MOVE WRK-ABEND        TO GFCT00-ABC-CICS

           COMPUTE WRK-EIB        = EIBRESP * 1
           MOVE WRK-EIB          TO GFCT00-EIB-CICS

           MOVE 'ERRO'           TO GFCT00-FUN-CICS
           MOVE 01               TO GFCT00-LOC-CICS
           MOVE 'ERRO CICS   '   TO GFCT00-MSG-CICS

           MOVE 2                TO GFCTER-COD-RETORNO
           MOVE 'X'              TO GFCTER-CTPO-MSGEM
           MOVE GFCT00-ERRO(1:70)      TO GFCTER-RMSG-INFOR
           MOVE GFCTER-RMSG-INFOR
                                 TO GFCTWAT3-RMSG-INFOR
           PERFORM   9992-FINALIZAR-PROCESSO
           .
      *----------------------------------------------------------------*
       9990-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9991-FORMATAR-MENSAGEM-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS            TO GFCTER-COD-RETORNO

           EXEC CICS
                LINK PROGRAM  ('GFCT1V00')
                     COMMAREA (GFCTER-AREA)
                     LENGTH   (LENGTH OF GFCTER-AREA)
           END-EXEC

           IF  GFCTER-COD-RETORNO  NOT  EQUAL ZEROS
               MOVE GFCTER-COD-RETORNO
                                 TO GFCTWAT3-COD-RETORNO
               MOVE GFCTER-NIVEL-MSGEM
                                 TO GFCTWAT3-NIVEL-MSGEM
               MOVE GFCTER-RMSG-INFOR
                                 TO GFCTWAT3-RMSG-INFOR
           ELSE
               MOVE GFCTER-RMSG-INFOR
                                 TO GFCTWAT3-RMSG-INFOR
           END-IF
           .
      *----------------------------------------------------------------*
       9991-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9992-FINALIZAR-PROCESSO        SECTION.
      *----------------------------------------------------------------*
           IF GFCTWAT3-E-COD-VERSAO EQUAL 001
              MOVE LNK-AREA-GFCTWAT3     TO   DFHCOMMAREA(1:EIBCALEN)
           ELSE
CI0416        IF GFCTWAT3-E-COD-VERSAO EQUAL 002
                 PERFORM 8000-TRATA-VERSAO-002
                 MOVE WRK-AREA-GFCTWAT6    TO DFHCOMMAREA(1:EIBCALEN)
              ELSE
                 MOVE WRK-AREA-GFCTWBE1    TO DFHCOMMAREA(1:EIBCALEN)
021116        END-IF
           END-IF.

           EXEC CICS
                RETURN
           END-EXEC
           .
      *----------------------------------------------------------------*
       9992-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
