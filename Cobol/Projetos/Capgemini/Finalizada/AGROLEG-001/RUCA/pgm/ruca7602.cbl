      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. RUCA7602.
       AUTHOR. HERIBERTO GIANNASI.
      *================================================================*
      *                      7  C  O  M  M                             *
      *================================================================*
      * PROGRAMA.....:  RUCA7601                                       *
      * ANALISTA.....:  HERIBERTO GIANNASI                             *
      * DATA.........:  26/01/2026                                     *
      *----------------------------------------------------------------*
      * OBJETIVO.....:  RECEBER CONTRATO (CCONTR) E RETORNAR           *
      *                 SALDOS DEVEDOR (PRINCIPAL/ENCARGOS/TOTAL)      *
      *----------------------------------------------------------------*
      * BOOK'S.......:  SQLCA                                          *
      * TABELAS......:                              INCLUDE            *
      *                 DB2PRD.RUCA_CAD_ROOT
      *                 DB2PRD.V01PARCELA_PGTO                         *
      * MODULOS......:  -                                              *
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
           '** RUCA6690 - AREA DE WORKING **'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-FIM-CSR                 PIC  X(001)         VALUE SPACES.

       01  IND-1                       PIC 9(003) COMP-3    VALUE ZEROS.

       01 WRK-ACU-SDVATU               PIC S9(013)V9(02)    COMP-3
                                                            VALUE ZEROS.
       01 WRK-ACU-PRINCIPAL            PIC S9(013)V9(02)    COMP-3
                                                            VALUE ZEROS.
       01 WRK-ACU-ENCARGOS             PIC S9(013)V9(02)    COMP-3
                                                            VALUE ZEROS.
       01 WRK-ACU-TPCREM              PIC S9(009)          COMP-3
                                                           VALUE ZEROS.

7C2603*01 WRK-VLJUR-RUMO               PIC S9(013)V9(02)    COMP-3
7C2603*                                                     VALUE ZEROS.

       01 WRK-S9-13V2                  PIC +9(13)V9(02)     VALUE ZEROS.
       01 FILLER                       REDEFINES WRK-S9-13V2.
          05 FILLER                    PIC X(01).
          05 WRK-9-13V2                PIC 9(13)V9(02).

       01 WRK-S9-07                    PIC +9(07)           VALUE ZEROS.
       01 FILLER                       REDEFINES WRK-S9-07.
          05 FILLER                    PIC  X(01).
          05 WRK-9-07                  PIC  9(07).
          05 FILLER                    REDEFINES WRK-9-07.
             10 FILLER                 PIC  9(04).
             10 WRK-9-03               PIC  9(03).

      *----------------------------------------------------------------*
      *    Variaveis para filtrar parcelas vencidas e a vencer         *
      *    (regra ruca6690: vencida + VPCELA/VPROVS-JURO zerados =     *
      *     considerar liquidada e nao incluir na lista)                *
      *----------------------------------------------------------------*
       01 WRK-BYTE-LIGADO              PIC  X(001)         VALUE '1'.
       01 WRK-CONTR-LIQUIDADO          PIC  X(001)         VALUE SPACES.
       01  WRK-DT-DB2.
          05 WRK-DT-DB2-DD             PIC  9(002)         VALUE ZEROS.
          05 FILLER                    PIC  X(001)         VALUE SPACES.
          05 WRK-DT-DB2-MM             PIC  9(002)         VALUE ZEROS.
          05 FILLER                    PIC  X(001)         VALUE SPACES.
          05 WRK-DT-DB2-AAAA           PIC  9(004)         VALUE ZEROS.
       
       01 WRK-DATA-CORRENTE            PIC  X(010)         VALUE SPACES.
       01 WRK-DATA-CURR-AAAAMMDD.
          05 WRK-AAAA-CUR              PIC  9(004)         VALUE ZEROS.
          05 WRK-MM-CUR                PIC  9(002)         VALUE ZEROS.
          05 WRK-DD-CUR                PIC  9(002)         VALUE ZEROS.
       01 WRK-DATA-CURR-R              REDEFINES
                                      WRK-DATA-CURR-AAAAMMDD PIC 9(008).
       01 WRK-DATA-VCTO-AAAAMMDD.
          05 WRK-AAAA-VCT              PIC  9(004)         VALUE ZEROS.
          05 WRK-MM-VCT                PIC  9(002)         VALUE ZEROS.
          05 WRK-DD-VCT                PIC  9(002)         VALUE ZEROS.
       01 WRK-DATA-VCTO-R              REDEFINES
                                      WRK-DATA-VCTO-AAAAMMDD PIC 9(008).

       01  WRK-SITUACAO                 PIC  X(009)        VALUE SPACES.
       01  WRK-VLJUR-RUMO               PIC S9(013)V9(02)  COMP-3
                                                           VALUE ZEROS.
       01  WRK-VALOR-JUR-AUX           PIC S9(013)V9(02)  COMP-3
                                                          VALUE ZEROS.
       01  WRK-VL-ENC-PARCELA          PIC S9(013)V9(02)  COMP-3
                                                           VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA BRAD7100         ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '** AREA PARA VARIAVEIS DB2 **'.
      *----------------------------------------------------------------*


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DO DB2                                ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *


           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE RUCAB003
           END-EXEC.

           EXEC SQL
                INCLUDE RUMOV000
           END-EXEC.

           EXEC SQL
                INCLUDE RUCAB004
           END-EXEC.


           EXEC SQL
             DECLARE  CSR01-RUCAB004 CURSOR FOR
              SELECT  B004_CH_CONTR
                   ,  B004_CH_TIPO
                   ,  B004_CH_NUMERO
                   ,  B004_DTEVENTO
                   ,  B004_DTCONTAB
                   ,  B004_VLPRICOR
                   ,  B004_JURCORRI
                   ,  B004_CH_TIPO
                   ,  B004_CONTROLE
                FROM  DB2PRD.RUCA_CAD_DEP
               WHERE  B004_CH_CONTR = :RUCAB004.B004-CH-CONTR
                 AND  B004_VLPRICOR > :RUCAB004.B004-VLPRICOR
                 AND  B004_DEBCRED  = :RUCAB004.B004-DEBCRED
                 AND  B004_CODLANC  < :RUCAB004.B004-CODLANC
            ORDER BY  B004_DTEVENTO
                    , B004_CH_CONTR
                    , B004_CH_TIPO
                    , B004_CH_NUMERO
            WITH UR
           END-EXEC.

       01  WRK-NULL-01                 PIC S9(04) COMP VALUE ZEROS.
       01  WRK-NULL-02                 PIC S9(04) COMP VALUE ZEROS.

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LNK-AREA.
       COPY RUCAW602.

      *================================================================*
       PROCEDURE                       DIVISION USING LNK-AREA.
      *================================================================*
      *----------------------------------------------------------------*
       0000-INICIO                     SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF RUCAW602-E-CCONTR        NOT NUMERIC
           OR RUCAW602-E-CCONTR        EQUAL ZEROS
              MOVE 08                  TO RUCAW602-RET-CODE
              MOVE 'NUMERO DO CONTRATO INVALIDO'
                                       TO RUCAW602-RET-MENSAGEM
           END-IF.

           MOVE ZEROS                  TO RUCAW602-RET-CODE
                                          RUCAW602-S-SLD-DEVEDOR
                                          RUCAW602-S-SLD-ENCARGOS
                                          RUCAW602-S-SLD-TOTAL
                                          RUCAW602-S-NUM-PARCELAS.
           
           PERFORM 1100-VALIDAR-CONTRATO.
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-VALIDAR-CONTRATO           SECTION.
      *----------------------------------------------------------------*

           MOVE RUCAW602-E-CCONTR      TO B003-CH-CONTR
                                       OF RUCAB003.

           EXEC SQL
                SELECT B003_VLAUXIL_OPER9
                    + (B003_JURPGAC_PAG8  -  B003_JURPGORI_CAP9 )
                    + (B003_CORRPGAC_PAG8 -  B003_CORRPGOR_CAP9 )
                    , (B003_JURPGAC_PAG8  -  B003_JURPGORI_CAP9 )
                    , B003_JURPGORI_CAP9
                INTO  :RUCAB003.B003-VLAUXIL-OPER9
                    , :RUCAB003.B003-JURPGAC-PAG8
                    , :RUCAB003.B003-JURPGORI-CAP9
                FROM  DB2PRD.RUCA_CAD_ROOT
                WHERE B003_CH_CONTR      = :RUCAB003.B003-CH-CONTR
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB003'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3500'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE ERRO-AREA(1:LENGTH OF RUCAW602-RET-MENSAGEM)
                                       TO RUCAW602-RET-MENSAGEM
               MOVE 16                 TO RUCAW602-RET-CODE
               PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 01                  TO RUCAW602-RET-CODE
              MOVE 'CONTRATO NAO ENCONTRADO PARA TOTALIZACAO'
                                       TO RUCAW602-RET-MENSAGEM
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.   
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-CALCULAR-TOTAL-PCREM.

           PERFORM 2300-LISTAR-PARCELAS.

           PERFORM 2100-CALCULAR-SALDO-DEVATU.

           MOVE ZEROS                 TO RUCAW602-RET-CODE.
           MOVE 'SALDO RENEG CALCULADO COM SUCESSO'
                                      TO RUCAW602-RET-MENSAGEM.

      *----------------------------------------------------------------*
       2000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-CALCULAR-SALDO-DEVATU      SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-ACU-SDVATU
                                          WRK-ACU-PRINCIPAL
                                          WRK-ACU-ENCARGOS.

           
           PERFORM                     VARYING IND-1       
                                       FROM 1 
                                       BY 1 
                                       UNTIL IND-1       
                                       GREATER RUCAW602-S-NUM-PARCELAS
               COMPUTE WRK-ACU-SDVATU  =  WRK-ACU-SDVATU
                                       + RUCAW602-S-PCELA-VLRENC 
                                         (IND-1)

               COMPUTE WRK-ACU-PRINCIPAL = WRK-ACU-PRINCIPAL
                                         + RUCAW602-S-PCELA-VLRPRIN
                                           (IND-1)

               COMPUTE WRK-ACU-ENCARGOS = WRK-ACU-ENCARGOS
                                         + RUCAW602-S-PCELA-VLRENC
                                           (IND-1)
           END-PERFORM  

           MOVE WRK-ACU-PRINCIPAL      TO WRK-S9-13V2
           MOVE WRK-9-13V2             TO RUCAW602-S-SLD-DEVEDOR.

           MOVE WRK-ACU-ENCARGOS       TO WRK-S9-13V2
           MOVE WRK-9-13V2             TO RUCAW602-S-SLD-ENCARGOS.

           MOVE WRK-ACU-SDVATU         TO WRK-S9-13V2
           MOVE WRK-9-13V2             TO RUCAW602-S-SLD-TOTAL.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-CALCULAR-TOTAL-PCREM       SECTION.
      *----------------------------------------------------------------*

           MOVE RUCAW602-E-CCONTR      TO B004-CH-CONTR    OF RUCAB004.
           MOVE 0                      TO B004-VLPRICOR    OF RUCAB004.
           MOVE '6'                    TO B004-DEBCRED     OF RUCAB004.
           MOVE 23                     TO B004-CODLANC     OF RUCAB004.
           MOVE ZEROS                  TO WRK-ACU-TPCREM.

           EXEC SQL
                SELECT COUNT(*)
                INTO  :WRK-ACU-TPCREM
                FROM   DB2PRD.RUCA_CAD_DEP
                WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
                  AND  B004_VLPRICOR      > :RUCAB004.B004-VLPRICOR
                  AND  B004_DEBCRED       = :RUCAB004.B004-DEBCRED
                  AND  B004_CODLANC       < :RUCAB004.B004-CODLANC
                  AND  B004_CH_TIPO       >= 0
                  AND  B004_CH_NUMERO     >= 1
                  AND  B004_DTEVENTO      >= CURRENT DATE
                  AND  SUBSTR(B004_CONTROLE, 1, 1) < :WRK-BYTE-LIGADO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'COUNT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE ERRO-AREA(1:LENGTH OF RUCAW602-RET-MENSAGEM)
                                       TO RUCAW602-RET-MENSAGEM
               MOVE 16                 TO RUCAW602-RET-CODE
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-ACU-TPCREM          GREATER ZEROS
               COMPUTE WRK-VALOR-JUR-AUX = B003-JURPGAC-PAG8
                                     / WRK-ACU-TPCREM
           ELSE
               MOVE B003-JURPGAC-PAG8  TO WRK-VALOR-JUR-AUX
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2300-LISTAR-PARCELAS           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SET :WRK-DATA-CORRENTE = CURRENT DATE
           END-EXEC.

           MOVE WRK-DATA-CORRENTE     TO WRK-DT-DB2.
           MOVE WRK-DT-DB2-AAAA       TO WRK-AAAA-CUR
           MOVE WRK-DT-DB2-MM         TO WRK-MM-CUR
           MOVE WRK-DT-DB2-DD         TO WRK-DD-CUR

           PERFORM 2310-ABRIR-CSR01.

           PERFORM 2320-LER-CSR01.

           PERFORM                    UNTIL WRK-FIM-CSR
                                      EQUAL 'S'
               PERFORM 2340-MOVER-PCELA
               PERFORM 2320-LER-CSR01
           END-PERFORM.

           MOVE IND-1                  TO RUCAW602-S-NUM-PARCELAS.

           PERFORM 2330-FECHAR-CSR01.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2310-ABRIR-CSR01                SECTION.
      *----------------------------------------------------------------*
           MOVE RUCAW602-E-CCONTR      TO B004-CH-CONTR    OF RUCAB004.
           MOVE 0                      TO B004-VLPRICOR    OF RUCAB004.
           MOVE '6'                    TO B004-DEBCRED     OF RUCAB004.
           MOVE 23                     TO B004-CODLANC     OF RUCAB004.

           EXEC SQL
                OPEN CSR01-RUCAB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE ERRO-AREA(1:LENGTH OF RUCAW602-RET-MENSAGEM)
                                       TO RUCAW602-RET-MENSAGEM
               MOVE 16                 TO RUCAW602-RET-CODE
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2320-LER-CSR01                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUCAB004
                 INTO :RUCAB004.B004-CH-CONTR
                    , :RUCAB004.B004-CH-TIPO
                    , :RUCAB004.B004-CH-NUMERO
                    , :RUCAB004.B004-DTEVENTO
                             :WRK-NULL-01
                    , :RUCAB004.B004-DTCONTAB
                             :WRK-NULL-02
                    , :RUCAB004.B004-VLPRICOR
                    , :RUCAB004.B004-JURCORRI
                    , :RUCAB004.B004-CH-TIPO
                    , :RUCAB004.B004-CONTROLE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE ERRO-AREA(1:LENGTH OF RUCAW602-RET-MENSAGEM)
                                       TO RUCAW602-RET-MENSAGEM
               MOVE 16                 TO RUCAW602-RET-CODE
               PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 'S'                 TO WRK-FIM-CSR
           ELSE
              IF WRK-NULL-01           LESS ZEROS
                 MOVE SPACES           TO B004-DTEVENTO
                                       OF RUCAB004
              END-IF
              IF WRK-NULL-02           LESS ZEROS
                 MOVE SPACES           TO B004-DTCONTAB
                                       OF RUCAB004
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2330-FECHAR-CSR01               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUCAB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCAB004'         TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE ERRO-AREA(1:LENGTH OF RUCAW602-RET-MENSAGEM)
                                       TO RUCAW602-RET-MENSAGEM
               MOVE 16                 TO RUCAW602-RET-CODE
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
       2340-MOVER-PCELA                SECTION.
      *----------------------------------------------------------------*
      *    Incluir na lista apenas parcelas vencidas e a vencer.        *
      *    Calculo do encargo alinhado ao fluxo do ruca6690.            *
      *----------------------------------------------------------------*

           PERFORM 2341-VERIFICAR-RUMO.

           COMPUTE WRK-VLJUR-RUMO      = WRK-VLJUR-RUMO
                                       + B004-JURCORRI OF RUCAB004

           MOVE 'S'                    TO WRK-CONTR-LIQUIDADO.
           MOVE SPACES                 TO WRK-SITUACAO.
           IF (B004-CH-TIPO            OF RUCAB004
                                       NOT LESS ZEROS)
           AND (B004-CH-NUMERO         OF RUCAB004
                                       NOT LESS 1)
               IF (B004-CONTROLE       OF RUCAB004 (1:1)
                                       LESS WRK-BYTE-LIGADO)
                   MOVE 'N'            TO WRK-CONTR-LIQUIDADO
               END-IF
           END-IF.

           PERFORM 2342-FORMATAR-SITUACAO.

           IF WRK-SITUACAO             EQUAL 'LIQUIDADA'
               GO TO 2340-99-FIM
           END-IF.

           ADD 1                       TO IND-1

           MOVE B004-CH-NUMERO         OF RUCAB004
                                       TO WRK-S9-07
           MOVE WRK-9-03               TO RUCAW602-S-PCELA-NUM
                                          (IND-1).

           MOVE B004-DTEVENTO          OF RUCAB004
                                       TO RUCAW602-S-PCELA-DTEVENTO
                                          (IND-1).
           MOVE B004-DTCONTAB          OF RUCAB004
                                       TO RUCAW602-S-PCELA-DTVCTO
                                          (IND-1).

           MOVE B004-VLPRICOR          OF RUCAB004
                                       TO WRK-S9-13V2
           MOVE WRK-9-13V2             TO RUCAW602-S-PCELA-VLRPRIN
                                          (IND-1).

           IF  WRK-VLJUR-RUMO          GREATER ZEROS
             IF WRK-SITUACAO           EQUAL 'A VENCER'
               COMPUTE WRK-VLJUR-RUMO = WRK-VLJUR-RUMO
                                      + WRK-VALOR-JUR-AUX
             END-IF
MJ0001       MOVE WRK-VLJUR-RUMO       TO WRK-S9-13V2
           ELSE
             MOVE B003-JURPGORI-CAP9   TO WRK-VL-ENC-PARCELA
             IF WRK-SITUACAO           EQUAL 'A VENCER'
               MOVE WRK-VALOR-JUR-AUX   TO WRK-VL-ENC-PARCELA
             END-IF

             MOVE WRK-VL-ENC-PARCELA   TO WRK-S9-13V2

           END-IF.

           MOVE WRK-9-13V2             TO RUCAW602-S-PCELA-VLRENC
                                          (IND-1).

           MOVE B004-CODLANC           OF RUCAB004
                                       TO RUCAW602-S-PCELA-CODLANC
                                          (IND-1).

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
       2341-VERIFICAR-RUMO            SECTION.
      *----------------------------------------------------------------*
           INITIALIZE RUMOV000

           MOVE B004-CH-CONTR         TO CCONTR           OF RUMOV000.
           MOVE B004-DTEVENTO         OF RUCAB004
                                      TO DVCTO            OF RUMOV000.

           EXEC SQL
                SELECT VPCELA
                      ,VPROVS_JURO
                      ,VJURO_REMUN_ATRSO
                      ,VJURO_MORA_PCELA
                      ,VMULTA_CONTR_ATRSO
                      ,VJURO_CREDT_ATRSO
                      ,VJURO_MORA_CREDT
                      ,VMULTA_CREDT_ATRSO
                INTO  :RUMOV000.VPCELA
                     ,:RUMOV000.VPROVS-JURO
                     ,:RUMOV000.VJURO-REMUN-ATRSO
                     ,:RUMOV000.VJURO-MORA-PCELA
                     ,:RUMOV000.VMULTA-CONTR-ATRSO
                     ,:RUMOV000.VJURO-CREDT-ATRSO
                     ,:RUMOV000.VJURO-MORA-CREDT
                     ,:RUMOV000.VMULTA-CREDT-ATRSO
                FROM   DB2PRD.V01PARCELA_PGTO
                WHERE  CCONTR     =  :RUMOV000.CCONTR
                AND    DVCTO      =  :RUMOV000.DVCTO
                WITH   UR
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUMOV000'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2341'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE ERRO-AREA(1:LENGTH OF RUCAW602-RET-MENSAGEM)
                                       TO RUCAW602-RET-MENSAGEM
               MOVE 16                 TO RUCAW602-RET-CODE
               PERFORM 3000-FINALIZAR
            END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO WRK-VLJUR-RUMO
           ELSE
               COMPUTE WRK-VLJUR-RUMO = VJURO-REMUN-ATRSO  OF RUMOV000
                                      + VJURO-MORA-PCELA   OF RUMOV000
                                      + VMULTA-CONTR-ATRSO OF RUMOV000
                                      + VJURO-CREDT-ATRSO  OF RUMOV000
                                      + VJURO-MORA-CREDT   OF RUMOV000
                                      + VMULTA-CREDT-ATRSO OF RUMOV000
           END-IF.

      *----------------------------------------------------------------*
       2341-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2342-FORMATAR-SITUACAO          SECTION.
      *----------------------------------------------------------------*

           MOVE B004-DTEVENTO          OF RUCAB004 
                                       TO WRK-DT-DB2                                       
           MOVE WRK-DT-DB2-AAAA        TO WRK-AAAA-VCT
           MOVE WRK-DT-DB2-MM          TO WRK-MM-VCT
           MOVE WRK-DT-DB2-DD          TO WRK-DD-VCT

           IF WRK-CONTR-LIQUIDADO      EQUAL 'S'
               MOVE 'LIQUIDADA'        TO WRK-SITUACAO
           ELSE
               IF WRK-DATA-VCTO-R      LESS WRK-DATA-CURR-R
                   IF VPCELA           OF RUMOV000 EQUAL ZEROS
                   AND VPROVS-JURO     OF RUMOV000 EQUAL ZEROS
                       MOVE 'LIQUIDADA' TO WRK-SITUACAO
                   ELSE
                       MOVE 'VENCIDA'  TO WRK-SITUACAO
                   END-IF
               ELSE
                   MOVE 'A VENCER'     TO WRK-SITUACAO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2342-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           GOBACK.
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
