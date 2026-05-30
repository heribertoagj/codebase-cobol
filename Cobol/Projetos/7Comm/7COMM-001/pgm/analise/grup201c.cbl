      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GRUP201C.
       AUTHOR.     TALASSA IOSHIMOTO.
      *================================================================*
      *                      B R A D E S C O                           *
      *----------------------------------------------------------------*
      *   PROGRAMA    : GRUP201C.                                      *
      *   ANALISTA    : TALASSA IOSHIMOTO                              *
      *   DATA        : 23/10/2025                                     *
      *----------------------------------------------------------------*
      *   OBJETIVO    : CONSULTAR A EMPRESA/PESSOA FISICA POR          *
      *                'GRUPO E CGC'  OU  SO POR 'CGC'                 *
      *----------------------------------------------------------------*
      *   BASES DB2   :                                                *
      *   EMPR_GRP                                 GRUPB002            *
      *   GRP_ECONM                                GRUPB001            *
      *----------------------------------------------------------------*
      *   BOOKS FUNCIONAIS:                                            *
      *   GRUPW01S - BOOK DE ACESSO AO SERVICO FUNCIONAL               *
      *----------------------------------------------------------------*
      *   MODULOS     :                                                *
      *   POOL1050                                                     *
      *   POOL7100                                                     *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *================================================================*
       CONFIGURATION                   SECTION.
      *================================================================*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*

      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050)     VALUE
           '*** INICIO DA WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050)     VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77 WRK-ERR-GRUPO                PIC  X(107)     VALUE
           '*** GRUPO NAO ATIVO - SITUACAO DIFERENTE DE 2 ***'.

       01 WRK-COD-USUARIO              PIC  X(007)     VALUE SPACES.

       01 WRK-USUARIO                  REDEFINES WRK-COD-USUARIO
                                       PIC  9(007).
      *
      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050)     VALUE
           '*** AREA DE NULIDADE ***'.
      *----------------------------------------------------------------*
       01 INDICADOR-DE-NULIDADE.
          03 WRK-CINCPC-EMPR-N         PIC S9(04) COMP VALUE ZEROS.
          03 WRK-DINCPC-EMPR-N         PIC S9(04) COMP VALUE ZEROS.
          03 WRK-RFONTE-INFO-INCPC-N   PIC S9(04) COMP VALUE ZEROS.
          03 WRK-CCGC-INCPR-ST-N       PIC S9(04) COMP VALUE ZEROS.
          03 WRK-CFLIAL-INCPR-ST-N     PIC S9(04) COMP VALUE ZEROS.
          03 WRK-CCTRL-INCPR-ST-N      PIC S9(04) COMP VALUE ZEROS.
          03 WRK-CID-PSSOA-N           PIC S9(04) COMP VALUE ZEROS.
          03 WRK-CCTRL-ID-PSSOA-N      PIC S9(04) COMP VALUE ZEROS.
          03 WRK-CIDTFD-MIDIA-N        PIC S9(04) COMP VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050)     VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GRUPB001
           END-EXEC.

           EXEC SQL
                INCLUDE GRUPB002
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           '*** AREA PARA O MODULO POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050)     VALUE
           '*** FIM DA WORKING-STORAGE GRUP201C ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GRUPW01S'.

       01 LNK-IO-PCB.
          05 LNK-IO-TERM.
             10  FILLER                PIC  X(001).
             10  LNK-IO-TERM2          PIC  X(007).
          05 FILLER                    PIC  X(002).
          05 LNK-IO-STATUS             PIC  X(002).
          05 FILLER                    PIC  X(012).
          05 LNK-IO-MODNAME            PIC  X(008).

       01 LNK-ALT-PCB.
          05 LNK-ALT-TERM              PIC  X(008).
          05 FILLER                    PIC  X(002).
          05 LNK-ALT-STATUS            PIC  X(002).
          05 FILLER                    PIC  X(012).
          05 LNK-ALT-MODNAME           PIC  X(008).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING GRUPW01S-REG-EMPRESA
                                LNK-IO-PCB  LNK-ALT-PCB.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0000-INICIO                     SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'

           MOVE ZEROS                  TO GRUPW01S-RETURN-CODE.

           PERFORM 1000-PROCESSA.

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSA                   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GRUPW01S-AREA-RETORNO.

           IF (GRUPW01S-PGM-CHAMADOR   EQUAL SPACES) OR
              (GRUPW01S-NUMERO-GRUPO-ENV
                                       NOT NUMERIC) OR
              (GRUPW01S-CGC-CPF        EQUAL SPACES) OR
              (GRUPW01S-CGC-CPF        EQUAL ZEROS) OR
              (GRUPW01S-FILIAL-CGC-CPF EQUAL SPACES)
              MOVE 004                 TO GRUPW01S-RETURN-CODE
              GOBACK
           END-IF

           IF  GRUPW01S-NUMERO-GRUPO-ENV
                                       GREATER ZEROS
               MOVE GRUPW01S-NUMERO-GRUPO-ENV
                                       TO GRUPW01S-NUMERO-GRUPO-RET
               PERFORM 1030-VERIFICAR-GRUPO-ATIVO

               IF  CSIT-GRP-ECONM      OF GRUPB001
                                       EQUAL 2
                   IF  GRUPW01S-FILIAL-CGC-CPF
                                       EQUAL ZEROS
                       PERFORM 1050-ACESSAR-GRUPO-CPF
                   ELSE
                       PERFORM 1060-ACESSAR-GRUPO-CGC
                   END-IF
               ELSE
                   MOVE 004            TO GRUPW01S-RETURN-CODE
                   MOVE WRK-ERR-GRUPO  TO GRUPW01S-ERRO-AREA
                   GOBACK
               END-IF
           ELSE
               IF  GRUPW01S-FILIAL-CGC-CPF
                                       EQUAL ZEROS
                   PERFORM 1100-ACESSAR-GRUPB002-CPF
               ELSE
                   PERFORM 1110-ACESSAR-GRUPB002-CGC
               END-IF
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 1150-OBTER-DADOS-GRUPO
               PERFORM 1200-FORMATAR-RETORNO
           ELSE
               MOVE 001                TO GRUPW01S-RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1030-VERIFICAR-GRUPO-ATIVO      SECTION.
      *----------------------------------------------------------------*

           MOVE GRUPW01S-NUMERO-GRUPO-ENV
                                       TO CGRP-ECONM
                                       OF GRUPB001

           EXEC SQL
             SELECT CGRP_ECONM,
                    CSIT_GRP_ECONM
               INTO
                    :GRUPB001.CGRP-ECONM,
                    :GRUPB001.CSIT-GRP-ECONM
               FROM DB2PRD.GRP_ECONM
              WHERE CGRP_ECONM = :GRUPB001.CGRP-ECONM
            END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'GRP_ECONM '       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0001'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 7000-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1030-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1050-ACESSAR-GRUPO-CPF          SECTION.
      *----------------------------------------------------------------*

           MOVE GRUPW01S-NUMERO-GRUPO-ENV
                                       TO CGRP-ECONM
                                       OF GRUPB002

           MOVE GRUPW01S-CGC-CPF       TO CCGC-CPF-GRP-ST
                                       OF GRUPB002

           MOVE ZEROS                  TO CFLIAL-CGC-GRP-ST
                                       OF GRUPB002.

           EXEC SQL
             SELECT CGRP_ECONM       ,
                    CCGC_CPF_GRP_ST  ,
                    CFLIAL_CGC_GRP_ST,
                    CCTRL_CGC_CPF_ST ,
                    IEMPR_GRP        ,
                    CTPO_PSSOA       ,
                    CISENC_CGC_CPF   ,
                    CSTPO_GRP_ECONM  ,
                    DINCL_REG        ,
                    CINCPC_EMPR      ,
                    DINCPC_EMPR      ,
                    RFONTE_INFO_INCPC,
                    CCGC_INCPR_ST    ,
                    CFLIAL_INCPR_ST  ,
                    CCTRL_INCPR_ST   ,
                    CID_PSSOA        ,
                    CCTRL_ID_PSSOA   ,
                    DULT_ALT_REG     ,
                    HULT_ALT_REG     ,
                    CUSUAR_SENHA     ,
                    CIDTFD_MIDIA
               INTO
                   :GRUPB002.CGRP-ECONM        ,
                   :GRUPB002.CCGC-CPF-GRP-ST   ,
                   :GRUPB002.CFLIAL-CGC-GRP-ST ,
                   :GRUPB002.CCTRL-CGC-CPF-ST  ,
                   :GRUPB002.IEMPR-GRP         ,
                   :GRUPB002.CTPO-PSSOA        ,
                   :GRUPB002.CISENC-CGC-CPF    ,
                   :GRUPB002.CSTPO-GRP-ECONM   ,
                   :GRUPB002.DINCL-REG         ,
                   :GRUPB002.CINCPC-EMPR
                     :WRK-CINCPC-EMPR-N,
                   :GRUPB002.DINCPC-EMPR
                     :WRK-DINCPC-EMPR-N,
                   :GRUPB002.RFONTE-INFO-INCPC
                     :WRK-RFONTE-INFO-INCPC-N,
                   :GRUPB002.CCGC-INCPR-ST
                     :WRK-CCGC-INCPR-ST-N,
                   :GRUPB002.CFLIAL-INCPR-ST
                     :WRK-CFLIAL-INCPR-ST-N,
                   :GRUPB002.CCTRL-INCPR-ST
                     :WRK-CCTRL-INCPR-ST-N,
                   :GRUPB002.CID-PSSOA
                     :WRK-CID-PSSOA-N,
                   :GRUPB002.CCTRL-ID-PSSOA
                     :WRK-CCTRL-ID-PSSOA-N,
                   :GRUPB002.DULT-ALT-REG,
                   :GRUPB002.HULT-ALT-REG,
                   :GRUPB002.CUSUAR-SENHA,
                   :GRUPB002.CIDTFD-MIDIA
                     :WRK-CIDTFD-MIDIA-N
               FROM DB2PRD.EMPR_GRP_ECONM
              WHERE CGRP_ECONM          = :GRUPB002.CGRP-ECONM
                AND CCGC_CPF_GRP_ST     = :GRUPB002.CCGC-CPF-GRP-ST
                AND CFLIAL_CGC_GRP_ST   = :GRUPB002.CFLIAL-CGC-GRP-ST
            END-EXEC.

            IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W' )
                 MOVE 'DB2'            TO ERR-TIPO-ACESSO
                 MOVE 'EMPR_GRP_ECONM' TO ERR-DBD-TAB
                 MOVE 'SELECT'         TO ERR-FUN-COMANDO
                 MOVE SQLCODE          TO ERR-SQL-CODE
                 MOVE '0002'           TO ERR-LOCAL
                 MOVE SPACES           TO ERR-SEGM
                 PERFORM 7000-ROTINA-ERRO
            END-IF
            .
      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1060-ACESSAR-GRUPO-CGC          SECTION.
      *----------------------------------------------------------------*

           MOVE GRUPW01S-NUMERO-GRUPO-ENV
                                       TO CGRP-ECONM
                                       OF GRUPB002

           MOVE GRUPW01S-CGC-CPF       TO CCGC-CPF-GRP-ST
                                       OF GRUPB002

           MOVE '0001'                 TO CFLIAL-CGC-GRP-ST
                                       OF GRUPB002.

           EXEC SQL
             SELECT CGRP_ECONM       ,
                    CCGC_CPF_GRP_ST  ,
                    CFLIAL_CGC_GRP_ST,
                    CCTRL_CGC_CPF_ST ,
                    IEMPR_GRP        ,
                    CTPO_PSSOA       ,
                    CISENC_CGC_CPF   ,
                    CSTPO_GRP_ECONM  ,
                    DINCL_REG        ,
                    CINCPC_EMPR      ,
                    DINCPC_EMPR      ,
                    RFONTE_INFO_INCPC,
                    CCGC_INCPR_ST    ,
                    CFLIAL_INCPR_ST  ,
                    CCTRL_INCPR_ST   ,
                    CID_PSSOA        ,
                    CCTRL_ID_PSSOA   ,
                    DULT_ALT_REG     ,
                    HULT_ALT_REG     ,
                    CUSUAR_SENHA     ,
                    CIDTFD_MIDIA
               INTO
                   :GRUPB002.CGRP-ECONM       ,
                   :GRUPB002.CCGC-CPF-GRP-ST  ,
                   :GRUPB002.CFLIAL-CGC-GRP-ST,
                   :GRUPB002.CCTRL-CGC-CPF-ST ,
                   :GRUPB002.IEMPR-GRP        ,
                   :GRUPB002.CTPO-PSSOA       ,
                   :GRUPB002.CISENC-CGC-CPF   ,
                   :GRUPB002.CSTPO-GRP-ECONM  ,
                   :GRUPB002.DINCL-REG        ,
                   :GRUPB002.CINCPC-EMPR
                     :WRK-CINCPC-EMPR-N,
                   :GRUPB002.DINCPC-EMPR
                     :WRK-DINCPC-EMPR-N,
                   :GRUPB002.RFONTE-INFO-INCPC
                     :WRK-RFONTE-INFO-INCPC-N,
                   :GRUPB002.CCGC-INCPR-ST
                     :WRK-CCGC-INCPR-ST-N,
                   :GRUPB002.CFLIAL-INCPR-ST
                     :WRK-CFLIAL-INCPR-ST-N,
                   :GRUPB002.CCTRL-INCPR-ST
                     :WRK-CCTRL-INCPR-ST-N,
                   :GRUPB002.CID-PSSOA
                     :WRK-CID-PSSOA-N,
                   :GRUPB002.CCTRL-ID-PSSOA
                     :WRK-CCTRL-ID-PSSOA-N,
                   :GRUPB002.DULT-ALT-REG,
                   :GRUPB002.HULT-ALT-REG,
                   :GRUPB002.CUSUAR-SENHA,
                   :GRUPB002.CIDTFD-MIDIA
                     :WRK-CIDTFD-MIDIA-N
               FROM DB2PRD.EMPR_GRP_ECONM
              WHERE CGRP_ECONM          = :GRUPB002.CGRP-ECONM
                AND CCGC_CPF_GRP_ST     = :GRUPB002.CCGC-CPF-GRP-ST
                AND CFLIAL_CGC_GRP_ST  >= :GRUPB002.CFLIAL-CGC-GRP-ST
                    ORDER  BY DULT_ALT_REG DESC
                    FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                       AND -811) OR
              (SQLWARN0                EQUAL 'W' )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'EMPR_GRP_ECONM '  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0003'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 7000-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1060-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-ACESSAR-GRUPB002-CPF       SECTION.
      *----------------------------------------------------------------*

           MOVE GRUPW01S-CGC-CPF       TO CCGC-CPF-GRP-ST
                                       OF GRUPB002

           MOVE ZEROS                  TO CFLIAL-CGC-GRP-ST
                                       OF GRUPB002.

           EXEC SQL
             SELECT A.CGRP_ECONM       ,
                    A.CCGC_CPF_GRP_ST  ,
                    A.CFLIAL_CGC_GRP_ST,
                    A.CCTRL_CGC_CPF_ST ,
                    A.IEMPR_GRP        ,
                    A.CTPO_PSSOA       ,
                    A.CISENC_CGC_CPF   ,
                    A.CSTPO_GRP_ECONM  ,
                    A.DINCL_REG        ,
                    A.CINCPC_EMPR      ,
                    A.DINCPC_EMPR      ,
                    A.RFONTE_INFO_INCPC,
                    A.CCGC_INCPR_ST    ,
                    A.CFLIAL_INCPR_ST  ,
                    A.CCTRL_INCPR_ST   ,
                    A.CID_PSSOA        ,
                    A.CCTRL_ID_PSSOA   ,
                    A.DULT_ALT_REG     ,
                    A.HULT_ALT_REG     ,
                    A.CUSUAR_SENHA     ,
                    A.CIDTFD_MIDIA
               INTO
                   :GRUPB002.CGRP-ECONM       ,
                   :GRUPB002.CCGC-CPF-GRP-ST  ,
                   :GRUPB002.CFLIAL-CGC-GRP-ST,
                   :GRUPB002.CCTRL-CGC-CPF-ST ,
                   :GRUPB002.IEMPR-GRP        ,
                   :GRUPB002.CTPO-PSSOA       ,
                   :GRUPB002.CISENC-CGC-CPF   ,
                   :GRUPB002.CSTPO-GRP-ECONM  ,
                   :GRUPB002.DINCL-REG        ,
                   :GRUPB002.CINCPC-EMPR
                     :WRK-CINCPC-EMPR-N,
                   :GRUPB002.DINCPC-EMPR
                     :WRK-DINCPC-EMPR-N,
                   :GRUPB002.RFONTE-INFO-INCPC
                     :WRK-RFONTE-INFO-INCPC-N,
                   :GRUPB002.CCGC-INCPR-ST
                     :WRK-CCGC-INCPR-ST-N,
                   :GRUPB002.CFLIAL-INCPR-ST
                     :WRK-CFLIAL-INCPR-ST-N,
                   :GRUPB002.CCTRL-INCPR-ST
                     :WRK-CCTRL-INCPR-ST-N,
                   :GRUPB002.CID-PSSOA
                     :WRK-CID-PSSOA-N,
                   :GRUPB002.CCTRL-ID-PSSOA
                     :WRK-CCTRL-ID-PSSOA-N,
                   :GRUPB002.DULT-ALT-REG,
                   :GRUPB002.HULT-ALT-REG,
                   :GRUPB002.CUSUAR-SENHA,
                   :GRUPB002.CIDTFD-MIDIA
                     :WRK-CIDTFD-MIDIA-N
               FROM DB2PRD.EMPR_GRP_ECONM  A,
                    DB2PRD.GRP_ECONM       B
               WHERE A.CCGC_CPF_GRP_ST    = :GRUPB002.CCGC-CPF-GRP-ST
                 AND A.CFLIAL_CGC_GRP_ST  = :GRUPB002.CFLIAL-CGC-GRP-ST
                 AND A.CGRP_ECONM         = B.CGRP_ECONM
                 AND B.CSIT_GRP_ECONM     = 2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'DB2PRD.EMPR_GRP'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0004'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 7000-ROTINA-ERRO
           END-IF
           .

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-ACESSAR-GRUPB002-CGC       SECTION.
      *----------------------------------------------------------------*

           MOVE GRUPW01S-CGC-CPF       TO CCGC-CPF-GRP-ST
                                       OF GRUPB002

           MOVE '0001'                 TO CFLIAL-CGC-GRP-ST
                                       OF GRUPB002.

           EXEC SQL
             SELECT A.CGRP_ECONM       ,
                    A.CCGC_CPF_GRP_ST  ,
                    A.CFLIAL_CGC_GRP_ST,
                    A.CCTRL_CGC_CPF_ST ,
                    A.IEMPR_GRP        ,
                    A.CTPO_PSSOA       ,
                    A.CISENC_CGC_CPF   ,
                    A.CSTPO_GRP_ECONM  ,
                    A.DINCL_REG        ,
                    A.CINCPC_EMPR      ,
                    A.DINCPC_EMPR      ,
                    A.RFONTE_INFO_INCPC,
                    A.CCGC_INCPR_ST    ,
                    A.CFLIAL_INCPR_ST  ,
                    A.CCTRL_INCPR_ST   ,
                    A.CID_PSSOA        ,
                    A.CCTRL_ID_PSSOA   ,
                    A.DULT_ALT_REG     ,
                    A.HULT_ALT_REG     ,
                    A.CUSUAR_SENHA     ,
                    A.CIDTFD_MIDIA
               INTO
                   :GRUPB002.CGRP-ECONM       ,
                   :GRUPB002.CCGC-CPF-GRP-ST  ,
                   :GRUPB002.CFLIAL-CGC-GRP-ST,
                   :GRUPB002.CCTRL-CGC-CPF-ST ,
                   :GRUPB002.IEMPR-GRP        ,
                   :GRUPB002.CTPO-PSSOA       ,
                   :GRUPB002.CISENC-CGC-CPF   ,
                   :GRUPB002.CSTPO-GRP-ECONM  ,
                   :GRUPB002.DINCL-REG        ,
                   :GRUPB002.CINCPC-EMPR
                     :WRK-CINCPC-EMPR-N,
                   :GRUPB002.DINCPC-EMPR
                     :WRK-DINCPC-EMPR-N,
                   :GRUPB002.RFONTE-INFO-INCPC
                     :WRK-RFONTE-INFO-INCPC-N,
                   :GRUPB002.CCGC-INCPR-ST
                     :WRK-CCGC-INCPR-ST-N,
                   :GRUPB002.CFLIAL-INCPR-ST
                     :WRK-CFLIAL-INCPR-ST-N,
                   :GRUPB002.CCTRL-INCPR-ST
                     :WRK-CCTRL-INCPR-ST-N,
                   :GRUPB002.CID-PSSOA
                     :WRK-CID-PSSOA-N,
                   :GRUPB002.CCTRL-ID-PSSOA
                     :WRK-CCTRL-ID-PSSOA-N,
                   :GRUPB002.DULT-ALT-REG,
                   :GRUPB002.HULT-ALT-REG,
                   :GRUPB002.CUSUAR-SENHA,
                   :GRUPB002.CIDTFD-MIDIA
                     :WRK-CIDTFD-MIDIA-N
               FROM DB2PRD.EMPR_GRP_ECONM  A,
                    DB2PRD.GRP_ECONM       B
              WHERE A.CCGC_CPF_GRP_ST    = :GRUPB002.CCGC-CPF-GRP-ST
                AND A.CFLIAL_CGC_GRP_ST >= :GRUPB002.CFLIAL-CGC-GRP-ST
                AND A.CGRP_ECONM         = B.CGRP_ECONM
                AND B.CSIT_GRP_ECONM     = 2
                    ORDER  BY A.DULT_ALT_REG DESC
                    FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811)
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'DB2PRD.EMPR_GRP'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0005'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 7000-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-OBTER-DADOS-GRUPO          SECTION.
      *----------------------------------------------------------------*

           MOVE CGRP-ECONM             OF GRUPB002
                                       TO CGRP-ECONM
                                       OF GRUPB001

           EXEC SQL
             SELECT CGRP_ECONM             ,
                    CTPO_GRP_ECONM         ,
                    IGRP_ECONM             ,
                    CSIT_GRP_ECONM         ,
                    CSTPO_GRP_ECONM        ,
                    DVALDD_GRP
               INTO
                   :GRUPB001.CGRP-ECONM    ,
                   :GRUPB001.CTPO-GRP-ECONM,
                   :GRUPB001.IGRP-ECONM    ,
                   :GRUPB001.CSIT-GRP-ECONM,
                   :GRUPB001.CSTPO-GRP-ECONM,
                   :GRUPB001.DVALDD-GRP
               FROM DB2PRD.GRP_ECONM
              WHERE CGRP_ECONM          = :GRUPB001.CGRP-ECONM
            END-EXEC.

            IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W' )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'GRP_ECONM'       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0006'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 7000-ROTINA-ERRO
            END-IF
            .
      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-FORMATAR-RETORNO           SECTION.
      *----------------------------------------------------------------*

           MOVE CGRP-ECONM             OF GRUPB002
                                       TO GRUPW01S-NUMERO-GRUPO-RET
           MOVE IEMPR-GRP              OF GRUPB002
                                       TO GRUPW01S-NOME-EMPRESA
           MOVE CTPO-PSSOA             OF GRUPB002
                                       TO GRUPW01S-TIPO-PESSOA
           MOVE CISENC-CGC-CPF         OF GRUPB002
                                       TO GRUPW01S-ISENC-CGC-CPF
           MOVE CSTPO-GRP-ECONM        OF GRUPB002
                                       TO GRUPW01S-SUBTPO-GPOEMPR-ECONM
           MOVE DINCL-REG              OF GRUPB002
                                       TO GRUPW01S-DATA-INCLUSAO

           IF  WRK-CINCPC-EMPR-N       EQUAL -1
               MOVE ZEROS              TO GRUPW01S-INCOR-EMPRESA
           ELSE
               MOVE CINCPC-EMPR        OF GRUPB002
                                       TO GRUPW01S-INCOR-EMPRESA
           END-IF

           IF  WRK-DINCPC-EMPR-N       EQUAL -1
               MOVE ZEROS              TO GRUPW01S-DATA-INCOR-EMPRESA
           ELSE
               MOVE DINCPC-EMPR        OF GRUPB002
                                       TO GRUPW01S-DATA-INCOR-EMPRESA
           END-IF

           IF  WRK-RFONTE-INFO-INCPC-N EQUAL -1
               MOVE SPACES             TO GRUPW01S-DS-FONTE-INFOR-INCOR
           ELSE
               MOVE RFONTE-INFO-INCPC  OF GRUPB002
                                       TO GRUPW01S-DS-FONTE-INFOR-INCOR
           END-IF

           IF  WRK-CCGC-INCPR-ST-N     EQUAL -1
               MOVE ZEROS              TO GRUPW01S-CGC-ICORPORADORA
           ELSE
               MOVE CCGC-INCPR-ST      OF GRUPB002
                                       TO GRUPW01S-CGC-ICORPORADORA
           END-IF

           IF  WRK-CFLIAL-INCPR-ST-N   EQUAL -1
               MOVE ZEROS              TO GRUPW01S-FILIAL-CGC-INCORP
           ELSE
               MOVE CFLIAL-INCPR-ST    OF GRUPB002
                                       TO GRUPW01S-FILIAL-CGC-INCORP
           END-IF

           IF  WRK-CCTRL-INCPR-ST-N    EQUAL -1
               MOVE ZEROS              TO GRUPW01S-CTRL-CGC-INCORP
           ELSE
               MOVE CCTRL-INCPR-ST     OF GRUPB002
                                       TO GRUPW01S-CTRL-CGC-INCORP
           END-IF

           IF  WRK-CID-PSSOA-N         EQUAL -1
               MOVE ZEROS              TO GRUPW01S-IDENT-PESSOA
           ELSE
               MOVE CID-PSSOA          OF GRUPB002
                                       TO GRUPW01S-IDENT-PESSOA
           END-IF

           IF  WRK-CCTRL-ID-PSSOA-N    EQUAL -1
               MOVE ZEROS              TO GRUPW01S-CTRL-IDENT-PESSOA
           ELSE
               MOVE CCTRL-ID-PSSOA     OF GRUPB002
                                       TO GRUPW01S-CTRL-IDENT-PESSOA
           END-IF

           MOVE DULT-ALT-REG           OF GRUPB002
                                       TO GRUPW01S-DATA-ULT-ALTER-REG

           MOVE HULT-ALT-REG           OF GRUPB002
                                       TO GRUPW01S-HORA-ULT-ALTER-REG

           MOVE CUSUAR-SENHA           OF GRUPB002
                                       TO WRK-COD-USUARIO
           MOVE WRK-COD-USUARIO        TO GRUPW01S-COD-USUARIO-SENHA

           IF  WRK-CIDTFD-MIDIA-N      EQUAL -1
               MOVE SPACES             TO GRUPW01S-IDENTIFICADOR-MIDIA
           ELSE
               MOVE CIDTFD-MIDIA       OF GRUPB002
                                       TO GRUPW01S-IDENTIFICADOR-MIDIA
           END-IF

           MOVE CTPO-GRP-ECONM         OF GRUPB001
                                       TO GRUPW01S-TIPO-GRUPO

           MOVE IGRP-ECONM             OF GRUPB001
                                       TO GRUPW01S-NOME-GRUPO

           MOVE CSIT-GRP-ECONM         OF GRUPB001
                                       TO GRUPW01S-SIT-GRUPO

           MOVE CSTPO-GRP-ECONM        OF GRUPB001
                                       TO GRUPW01S-SUBTPO-GRUPO

           MOVE DVALDD-GRP             OF GRUPB001
                                       TO GRUPW01S-DATA-VALID-GRUPO
           .
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GRUP201C'             TO ERR-PGM.

           MOVE 999                    TO GRUPW01S-RETURN-CODE

           MOVE GRUPW01S-ERRO-AREA     TO GRUPW01S-ERRO-AREA
           MOVE SQLCA                  TO GRUPW01S-SQLCA
           GOBACK.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FINALIZAR-FIM.          EXIT.
      *----------------------------------------------------------------*
