----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
***************************** Top of Data ******************************
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5801.
       AUTHOR.     SONDA IT.
      *================================================================*
      *                   SONDA IT                                     *
      *----------------------------------------------------------------*
      *                                                                *
      *    CONSULTORIA  : SONDA IT                                     *
      *    PROGRAMA     : GFCT5801                                     *
      *    ANALISTA     : PAGNOCCA                                     *
      *    PROGRAMADOR  : PAGNOCCA                                     *
      *    DATA         : OUT/2015                                     *
      *                                                                *
      *    OBJETIVO     : EFETUAR DE/PARA DE CONTAS HSBC/BRADESCO      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.V01CLIENTE_AGENCIA   CLIEV004        *
      *                    DB2PRD.V01CTA_POUPANCA      CLIEV007        *
      *                    DB2PRD.V01CTA_CORRENTE      CLIEV008        *
      *                                                                *
      *================================================================*
      *                   ALTERACOES                                   *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

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
           '*  INICIO DA WORKING GFCT5801  *'.
      *----------------------------------------------------------------*
       77  WRK-BANCO                  PIC S9(03) COMP-3 VALUE 399.
       77  WRK-ACHOU                  PIC X(01)         VALUE SPACES.
       77  WRK-MODULO                 PIC X(08)      VALUE 'CLIB5602'.

       01 WRK-CONTA-DESCOMP           PIC +9(13)        VALUE ZEROS.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       01                   REDEFINES WRK-CONTA-DESCOMP.
          05                          PIC 9(07).
          05 WRK-CTA                  PIC 9(07).

       01  WRK-CGRP-CTBIL-ATUAL-C      PIC +9(003) VALUE ZEROS.
       01  REDEFINES WRK-CGRP-CTBIL-ATUAL-C.
           05                          PIC  X(002).
           05 WRK-CGRP-CTBIL-ATUAL-S   PIC  9(002).

       01  WRK-CSGRP-CTBIL-C           PIC +9(003) VALUE ZEROS.
       01  REDEFINES WRK-CSGRP-CTBIL-C.
           05                          PIC  X(001).
           05 WRK-CSGRP-CTBIL-S        PIC  9(003).

       01  WRK-RAZAO-BRAD              PIC  9(005) VALUE ZEROS.
       01  REDEFINES WRK-RAZAO-BRAD.
           05 WRK-CGRP-CTBIL-ATUAL     PIC  9(002).
           05 WRK-CSGRP-CTBIL          PIC  9(003).

       01 WRK-CLIB5602.
           03  WRK-PARAMETROS-ENTRADA.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               05  WRK-GRUPO-INC      PIC  S9(01)        VALUE ZEROS.
               05  WRK-BANCO-INC      PIC  S9(03) COMP-3 VALUE ZEROS.
               05  WRK-AGENCIA-INC    PIC  S9(05) COMP-3 VALUE ZEROS.
               05  WRK-CONTA-INC      PIC  S9(13) COMP-3 VALUE ZEROS.
               05  WRK-PAB-INC        PIC  S9(03) COMP-3 VALUE ZEROS.
               05  WRK-TR-INC         PIC  S9(05) COMP-3 VALUE ZEROS.
           03  WRK-PARAMETROS-SAIDA.
               05  WRK-BANCO-BDSCO    PIC  9(03)  COMP-3 VALUE ZEROS.
               05  WRK-AGENCIA-BDSCO  PIC  9(05)  COMP-3 VALUE ZEROS.
               05  WRK-CONTA-BDSCO    PIC  9(13)  COMP-3 VALUE ZEROS.
               05  WRK-DIG-CTA-BDSCO  PIC  X(02)        VALUE SPACES.
               05  WRK-RAZAO-BDSCO-1  PIC  9(05)  COMP-3 VALUE ZEROS.
               05  WRK-RAZAO-BDSCO-2  PIC  9(05)  COMP-3 VALUE ZEROS.
               05  WRK-GRUPO-BDSCO    PIC  9(03)  COMP-3 VALUE ZEROS.
               05  WRK-SUBGPO-BDSCO   PIC  9(03)  COMP-3 VALUE ZEROS.
               05  WRK-GEST-AGENCIA   PIC  9(05)  COMP-3 VALUE ZEROS.
               05  WRK-COD-RETORNO    PIC  9(02)         VALUE ZEROS.
               05  WRK-SQLCODE        PIC S9(05)  COMP-3 VALUE ZEROS.
               05  WRK-MENSAGEM       PIC  X(60)        VALUE SPACES.

       01  .
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           03  WRK-MSG-015        PIC  X(60)  VALUE
           'CHAVE PESQ BCO INCORPORADO NAO ENCONT. NA BASE DE CONVERSAO'
           .
           03  WRK-MSG-016        PIC  X(60)  VALUE
           'AGENCIA BCO INCORPORADO NAO ENCONTRADA NA TAB. DE AGENCIAS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

           EXEC SQL INCLUDE CLIEV004 END-EXEC.

           EXEC SQL INCLUDE CLIEV007 END-EXEC.

           EXEC SQL INCLUDE CLIEV008 END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           '*    FIM DA WORKING GFCT5801   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.

      *----------------------------------------------------------------*
      *
      *
      *----------------------------------------------------------------*
      *  AREA DE COMUNICACAO DE/PARA DE CONTAS HSBC                    *
      *  BOOK GFCTWF04                                                 *
      *  LRECL 0180                                                    *
      *----------------------------------------------------------------*
      * F04-TAR-COD-RET....- CODIGO DE RETORNO
      *  00 - DE/PARA OK
      *  04 - AG/CTA HSBC NAO LOCALIZADA
      *  08 - AG/CTA HSBC INVALIDA
      *  12 - ERRO SISTEMICO
      *----------------------------------------------------------------*
      *
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       01  LNK-DP-CONTAS-HSBC-REG.
           05 LNK-AGE-HSBC                 PIC  9(05).
           05 LNK-CTA-HSBC                 PIC  9(13).
           05 LNK-T-CTA-HSBC               PIC  9(01).
           05 LNK-PESQ-CLIE                PIC  X(01).
           05 LNK-AGE-BRAD                 PIC  9(05).
           05 LNK-CTA-BRAD                 PIC  9(07).
           05 LNK-FJ-BRAD                  PIC  X(01).
           05 LNK-CGC-BRAD                 PIC  9(09).
           05 LNK-FIL-BRAD                 PIC  9(05).
           05 LNK-CTR-BRAD                 PIC  9(02).
           05 LNK-POSTO-BRAD               PIC  9(05).
           05 LNK-SEGM-BRAD                PIC  9(05).
           05 LNK-NOME-BRAD                PIC  X(70).
           05 LNK-CIDCLI-BRAD              PIC  X(26).
           05 LNK-RAZAO-BRAD               PIC  9(05).
           05 LNK-COD-RET                  PIC  9(02).
           05 LNK-TAR-RESERVA              PIC  X(20).

      *================================================================*
       PROCEDURE DIVISION             USING LNK-DP-CONTAS-HSBC-REG.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *================================================================*
            CALL 'CKRS1000'.

      *----------------------------------------------------------------*
      *     ROTINA PRINCIPAL
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM  1000-INICIALIZACAO.
           PERFORM  2000-PROCESSAR.
           PERFORM  3000-FINALIZACAO.
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     INICIALIZACAO
      *----------------------------------------------------------------*
       1000-INICIALIZACAO             SECTION.
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           IF LNK-AGE-HSBC NOT NUMERIC OR
              LNK-AGE-HSBC EQUAL ZEROS OR
              LNK-CTA-HSBC NOT NUMERIC OR
              LNK-CTA-HSBC EQUAL ZEROS OR
             (LNK-T-CTA-HSBC NOT EQUAL 1 AND 2) OR
             (LNK-PESQ-CLIE  NOT EQUAL 'S' AND 'N')
              MOVE 8 TO LNK-COD-RET
              GOBACK
           END-IF.

           MOVE ZEROS                  TO WRK-BANCO-BDSCO
                                          WRK-AGENCIA-BDSCO
                                          WRK-CONTA-BDSCO
                                          WRK-RAZAO-BDSCO-1
                                          WRK-RAZAO-BDSCO-2
                                          WRK-GRUPO-BDSCO
                                          WRK-SUBGPO-BDSCO
                                          WRK-GEST-AGENCIA
                                          WRK-COD-RETORNO
                                          WRK-SQLCODE.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           MOVE SPACES                 TO WRK-DIG-CTA-BDSCO
                                          WRK-MENSAGEM.
           MOVE 'N' TO WRK-ACHOU.
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR
      *----------------------------------------------------------------*
       2000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           IF LNK-T-CTA-HSBC EQUAL TO  1
              MOVE  3              TO WRK-GRUPO-INC
           ELSE
              MOVE  1              TO WRK-GRUPO-INC
           END-IF.
           MOVE  WRK-BANCO             TO WRK-BANCO-INC.
           MOVE  LNK-AGE-HSBC          TO WRK-AGENCIA-INC.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           MOVE  LNK-CTA-HSBC          TO WRK-CONTA-INC.
           MOVE  ZEROS                 TO WRK-PAB-INC
                                          WRK-TR-INC
                                          WRK-COD-RETORNO.


           CALL WRK-MODULO             USING WRK-CLIB5602.


           IF  WRK-COD-RETORNO NOT EQUAL ZEROS
               IF (WRK-COD-RETORNO EQUAL 12 AND
                  (WRK-MENSAGEM EQUAL WRK-MSG-015 OR
                   WRK-MENSAGEM EQUAL WRK-MSG-016))
                  MOVE 4               TO LNK-COD-RET
               ELSE
                  MOVE WRK-COD-RETORNO TO LNK-COD-RET
               END-IF
               GO TO 2000-99-FIM
           END-IF.


----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           IF WRK-BANCO-BDSCO NOT EQUAL 237
               MOVE 8 TO LNK-COD-RET
               GO TO 2000-99-FIM
           END-IF.


           IF (WRK-GRUPO-BDSCO EQUAL  7 AND
               WRK-GRUPO-INC   EQUAL  3) OR
              (WRK-GRUPO-BDSCO EQUAL 10 AND
               WRK-GRUPO-INC   EQUAL  1)
               CONTINUE
           ELSE
               MOVE 8 TO LNK-COD-RET
               GO TO 2000-99-FIM
           END-IF.

           MOVE 'S' TO WRK-ACHOU
           MOVE ZEROS                  TO LNK-COD-RET
           MOVE WRK-AGENCIA-BDSCO      TO LNK-AGE-BRAD
           MOVE WRK-CONTA-BDSCO        TO WRK-CONTA-DESCOMP.
           MOVE WRK-CTA                TO LNK-CTA-BRAD
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           IF LNK-PESQ-CLIE  EQUAL 'N'
              MOVE SPACES              TO LNK-FJ-BRAD
              MOVE ZEROS               TO LNK-CGC-BRAD
              MOVE ZEROS               TO LNK-FIL-BRAD
              MOVE ZEROS               TO LNK-CTR-BRAD
              MOVE ZEROS               TO LNK-POSTO-BRAD
              MOVE ZEROS               TO LNK-SEGM-BRAD
              MOVE SPACES              TO LNK-NOME-BRAD
              MOVE ZEROS               TO LNK-CIDCLI-BRAD
              MOVE ZEROS               TO LNK-RAZAO-BRAD
              GO TO 2000-99-FIM
           END-IF.

           IF WRK-GRUPO-INC   EQUAL  1
              PERFORM 2100-ACESSA-CLIEV007
           ELSE
              PERFORM 2200-ACESSA-CLIEV008
           END-IF.

           PERFORM 2300-ACESSA-CLIEV004.

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *----------------------------------------------------------------*
       2000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ACESSA CLIEV007
      *----------------------------------------------------------------*
       2100-ACESSA-CLIEV007          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-BDSCO    TO CJUNC-DEPDC OF CLIEV007.
           MOVE WRK-CTA              TO CCTA-CLI    OF CLIEV007.

           EXEC SQL SELECT CID_CLI
                         , CPOSTO_SERVC
                         , CSGMTO_CLI
                         , CGRP_CTBIL_ATUAL
                         , CSGRP_CTBIL
                      INTO :CLIEV007.CID-CLI
                         , :CLIEV007.CPOSTO-SERVC
                         , :CLIEV007.CSGMTO-CLI
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
                         , :CLIEV007.CGRP-CTBIL-ATUAL
                         , :CLIEV007.CSGRP-CTBIL
                      FROM DB2PRD.V01CTA_POUPANCA
                     WHERE CJUNC_DEPDC = :CLIEV007.CJUNC-DEPDC
                       AND CCTA_CLI    = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0    EQUAL 'W')
              MOVE 12 TO LNK-COD-RET
              GOBACK
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 4  TO LNK-COD-RET
              GOBACK
           END-IF.

           MOVE CID-CLI          OF CLIEV007 TO LNK-CIDCLI-BRAD
           MOVE CPOSTO-SERVC     OF CLIEV007 TO LNK-POSTO-BRAD
           MOVE CSGMTO-CLI       OF CLIEV007 TO LNK-SEGM-BRAD
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           MOVE CGRP-CTBIL-ATUAL OF CLIEV007 TO WRK-CGRP-CTBIL-ATUAL-C.
           MOVE CSGRP-CTBIL      OF CLIEV007 TO WRK-CSGRP-CTBIL-C.
           MOVE WRK-CGRP-CTBIL-ATUAL-S       TO WRK-CGRP-CTBIL-ATUAL.
           MOVE WRK-CSGRP-CTBIL-S            TO WRK-CSGRP-CTBIL.
           MOVE WRK-RAZAO-BRAD               TO LNK-RAZAO-BRAD.

      *----------------------------------------------------------------*
       2100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ACESSA CLIEV008
      *----------------------------------------------------------------*
       2200-ACESSA-CLIEV008          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-BDSCO    TO CJUNC-DEPDC OF CLIEV008.
           MOVE WRK-CTA              TO CCTA-CLI    OF CLIEV008.

           EXEC SQL SELECT CID_CLI
                         , CPOSTO_SERVC
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
                         , CSGMTO_CLI
                         , CGRP_CTBIL_ATUAL
                         , CSGRP_CTBIL
                      INTO :CLIEV008.CID-CLI
                         , :CLIEV008.CPOSTO-SERVC
                         , :CLIEV008.CSGMTO-CLI
                         , :CLIEV008.CGRP-CTBIL-ATUAL
                         , :CLIEV008.CSGRP-CTBIL
                      FROM DB2PRD.V01CTA_CORRENTE
                     WHERE CJUNC_DEPDC = :CLIEV008.CJUNC-DEPDC
                       AND CCTA_CLI    = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0    EQUAL 'W')
              MOVE 12 TO LNK-COD-RET
              GOBACK
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 4  TO LNK-COD-RET
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
              GOBACK
           END-IF.

           MOVE CID-CLI          OF CLIEV008 TO LNK-CIDCLI-BRAD
           MOVE CPOSTO-SERVC     OF CLIEV008 TO LNK-POSTO-BRAD
           MOVE CSGMTO-CLI       OF CLIEV008 TO LNK-SEGM-BRAD
           MOVE CGRP-CTBIL-ATUAL OF CLIEV008 TO WRK-CGRP-CTBIL-ATUAL-C.
           MOVE CSGRP-CTBIL      OF CLIEV008 TO WRK-CSGRP-CTBIL-C.
           MOVE WRK-CGRP-CTBIL-ATUAL-S       TO WRK-CGRP-CTBIL-ATUAL.
           MOVE WRK-CSGRP-CTBIL-S            TO WRK-CSGRP-CTBIL.
           MOVE WRK-RAZAO-BRAD               TO LNK-RAZAO-BRAD.

      *----------------------------------------------------------------*
       2200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ACESSA CLIEV004
      *----------------------------------------------------------------*
       2300-ACESSA-CLIEV004          SECTION.
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           MOVE LNK-CIDCLI-BRAD      TO CID-CLI OF CLIEV004.

           EXEC SQL SELECT CTPO_PSSOA
                         , CCGC_CPF
                         , CFLIAL_CGC
                         , CCTRL_CPF_CGC
                         , IPSSOA_COPLT
                      INTO :CLIEV004.CTPO-PSSOA
                         , :CLIEV004.CCGC-CPF
                         , :CLIEV004.CFLIAL-CGC
                         , :CLIEV004.CCTRL-CPF-CGC
                         , :CLIEV004.IPSSOA-COPLT
                      FROM DB2PRD.V01CLIENTE_AGENCIA
                     WHERE CID_CLI = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0    EQUAL 'W')
              MOVE 12 TO LNK-COD-RET
              GOBACK
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 4  TO LNK-COD-RET
              GOBACK
           END-IF.

           MOVE CTPO-PSSOA    OF CLIEV004 TO LNK-FJ-BRAD.
           MOVE CCGC-CPF      OF CLIEV004 TO LNK-CGC-BRAD.
           MOVE CFLIAL-CGC    OF CLIEV004 TO LNK-FIL-BRAD.
           MOVE CCTRL-CPF-CGC OF CLIEV004 TO LNK-CTR-BRAD.
           MOVE IPSSOA-COPLT  OF CLIEV004 TO LNK-NOME-BRAD.

      *----------------------------------------------------------------*
       2300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO
      *----------------------------------------------------------------*
       3000-FINALIZACAO              SECTION.
