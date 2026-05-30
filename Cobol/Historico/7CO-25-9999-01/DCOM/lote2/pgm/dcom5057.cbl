      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID.     DCOM5057.
       AUTHOR.         DUARTE.
       DATE-WRITTEN.   26-04-2006.
      *DATE-COMPILLED.
      *---------------------------------------------------------------*
      *  OBJETIVO..: EXCLUIR ISENCAO CEDENTE POR SUBPRODUTO           *
      *---------------------------------------------------------------*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** INICIO WORKING DCOM5057 ***'.
      *---------------------------------------------------------------*

       01  WRK-CCHAVE-ELMTO-DESC.
           05  WRK-CCHAVE-ELMTO-DESC-1 PIC  9(003)         VALUE ZEROS.
           05  WRK-CCHAVE-ELMTO-DESC-2 PIC  9(003)         VALUE ZEROS.
4S2511*    05  WRK-CCHAVE-ELMTO-DESC-3 PIC  9(009)         VALUE ZEROS.
4S2511     05  WRK-CCHAVE-ELMTO-DESC-3 PIC  X(09)          VALUE SPACES.
           05  WRK-CCHAVE-ELMTO-DESC-5 PIC  X(010)         VALUE SPACES.
           05  WRK-CCHAVE-ELMTO-DESC-6 PIC  9(003)         VALUE ZEROS.
           05  FILLER                  PIC  X(017)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGENS ***'.
      *---------------------------------------------------------------*

       COPY 'I#DCOMSG'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM6744 ***'.
      *---------------------------------------------------------------*

       COPY 'I#DCOM48'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM6815/6819 ***'.
      *---------------------------------------------------------------*

       COPY 'I#DCOM63'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM5075 ***'.
      *---------------------------------------------------------------*

       COPY 'I#DCOMX1'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       01  WS-AUX.
           03 WRK-ERRO                 PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-MODULO               PIC  X(008)         VALUE SPACES.
           03 WRK-NSEQ-PSSOA-DESC      PIC  9(003)         VALUE ZEROS.
           03 WRK-TIMESTAMP-ATUAL      PIC  X(026)         VALUE SPACES.
           03 WRK-DFIM-VGCIA-ISENC     PIC  X(010)         VALUE SPACES.
           03 WRK-DATA-ATU             PIC  9(008)         VALUE ZEROS.
           03 WRK-DATA-INI             PIC  9(008)         VALUE ZEROS.
           03 WRK-DATA-FIM             PIC  9(008)         VALUE ZEROS.

      *---------------------------------------------------------------*
       LINKAGE                              SECTION.
      *---------------------------------------------------------------*

       01  ENTRADA-ROTEADOR.
           03 ROTENT-COD-RETORNO       PIC  X(004).
           03 ROTENT-MENSAGEM          PIC  X(079).
           03 ROTENT-RESTART           PIC  9(005).
           03 ROTENT-FLAG              PIC  X(001).
           03 ROTENT-CFUNC-BDSCO       PIC  9(009).
           03 ROTENT-CTERM             PIC  X(008).
4S2511*    03 ROTENT-CCNPJ-CPF         PIC  9(009).
4S2511     03 ROTENT-CCNPJ-CPF         PIC  X(09).
4S2511*    03 ROTENT-CFLIAL-CNPJ       PIC  9(004).
4S2511     03 ROTENT-CFLIAL-CNPJ       PIC  X(04).
           03 ROTENT-CCTRL-CNPJ-CPF    PIC  9(002).
           03 ROTENT-CTPO-ISENC-DESC   PIC  9(003).
           03 ROTENT-DINIC-VGCIA-ISENC PIC  X(010).
           03 ROTENT-CPRODT            PIC  9(003).
           03 ROTENT-CSPROD-DESC-COML  PIC  9(003).

       01  SAIDA-ROTEADOR.
           03 ROTSAI-HEADER.
             05 ROTSAI-COD-RETORNO     PIC  X(004).
             05 ROTSAI-MSG-RETORNO     PIC  X(079).
             05 ROTSAI-RESTART         PIC  9(005).
             05 ROTSAI-FLAG            PIC  X(001).

           03 ROTSAI-CONSISTENCIA.
             05 ROTSAI-TAMANHO-ERRO    PIC  9(001).
             05 ROTSAI-TABELA-ERROS    OCCURS 007  TIMES.
               07 ROTSAI-CAMPOS-ERRO   PIC  9(001).

       01  LKG-ERRO-AREA               PIC  X(107).
       01  LKG-SQLCA                   PIC  X(136).

      *================================================================*
       PROCEDURE DIVISION              USING ENTRADA-ROTEADOR
                                             SAIDA-ROTEADOR
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.
      *================================================================*

      *---------------------------------------------------------------
       000-000-INICIO                  SECTION.
      *---------------------------------------------------------------

           INITIALIZE SAIDA-ROTEADOR
                      WRK-CCHAVE-ELMTO-DESC
                      48-ARGUMENTOS-ENTRADA
                      48-RETORNO
                      63-ARGUMENTOS-ENTRADA
                      63-RETORNO
                      X1-ARGUMENTOS-ENTRADA
                      X1-RETORNO
                      LKG-ERRO-AREA
                      LKG-SQLCA.

           MOVE  1                     TO ROTSAI-TAMANHO-ERRO.
           MOVE  '0000'                TO ROTSAI-COD-RETORNO.
           MOVE  SG-MENSAGEM(001)      TO ROTSAI-MSG-RETORNO.

           PERFORM 005-000-CONSISTIR-ENTRADAS.

           PERFORM 010-000-TRATAR-NEGOCIO.

           PERFORM 999-999-FINALIZAR.

      *---------------------------------------------------------------
       000-000-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       005-000-CONSISTIR-ENTRADAS      SECTION.
      *---------------------------------------------------------------
      * CONSISTIR ENTRADAS

4S2511*    IF  ROTENT-CCNPJ-CPF        NOT NUMERIC OR
4S2511*        ROTENT-CCNPJ-CPF        EQUAL ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF        EQUAL SPACES OR
4S2511         ROTENT-CCNPJ-CPF        EQUAL LOW-VALUES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(1)
               MOVE '0223'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(223)   TO ROTSAI-MSG-RETORNO
               PERFORM  999-999-FINALIZAR
           END-IF.

           IF  ROTENT-CTPO-ISENC-DESC  NOT NUMERIC OR
              (ROTENT-CTPO-ISENC-DESC  NOT EQUAL 001 AND 002)
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(4)
               MOVE '0495'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(495)   TO ROTSAI-MSG-RETORNO
               PERFORM  999-999-FINALIZAR
           END-IF.

           IF  ROTENT-DINIC-VGCIA-ISENC EQUAL SPACES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(5)
               MOVE '0540'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(540)   TO ROTSAI-MSG-RETORNO
               PERFORM  999-999-FINALIZAR
           END-IF.

           IF  ROTENT-CPRODT           NOT NUMERIC OR
               ROTENT-CPRODT           EQUAL ZEROS
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(6)
               MOVE '0115'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(115)   TO ROTSAI-MSG-RETORNO
               PERFORM  999-999-FINALIZAR
           END-IF.

           IF  ROTENT-CSPROD-DESC-COML NOT NUMERIC OR
               ROTENT-CSPROD-DESC-COML EQUAL ZEROS
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(7)
               MOVE '0116'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(116)   TO ROTSAI-MSG-RETORNO
               PERFORM  999-999-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       005-000-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       010-000-TRATAR-NEGOCIO          SECTION.
      *---------------------------------------------------------------

           PERFORM 020-000-OBTER-TIMESTAMP.

           PERFORM 020-005-OBTER-DATA-LOG.

           PERFORM 020-006-OBTER-63-ANTERIOR.

           PERFORM 020-010-VALIDAR-EXCLUSAO.

           PERFORM 020-030-GRAVAR-LOG-SUBP.

           PERFORM 020-040-VALIDAR-EXCLUSAO-PSSOA.

           IF   63-QTDE-TOTAL          EQUAL ZEROS
                PERFORM 020-050-EXCLUIR-PSSOA
           END-IF.

      *---------------------------------------------------------------
       010-000-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-000-OBTER-TIMESTAMP        SECTION.
      *---------------------------------------------------------------

           MOVE  'DCOM7999'           TO WRK-MODULO.

           CALL  WRK-MODULO           USING WRK-TIMESTAMP-ATUAL
                                            LKG-ERRO-AREA
                                            LKG-SQLCA.

      *---------------------------------------------------------------
       020-000-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-005-OBTER-DATA-LOG          SECTION.
      *---------------------------------------------------------------

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF.
4S2511     MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF-ST.
4S2511*    MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ.
4S2511     MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ-ST.
           MOVE ROTENT-CCTRL-CNPJ-CPF   TO 63-CCTRL-CNPJ-CPF.
           MOVE ROTENT-CTPO-ISENC-DESC  TO 63-CTPO-ISENC-DESC.
           MOVE ROTENT-CPRODT           TO 63-CPRODT.
           MOVE ROTENT-CSPROD-DESC-COML TO 63-CSPROD-DESC-COML.
           MOVE 'PARCIAL-05'            TO 63-INSTRUCAO.
           MOVE 'DCOM6815'              TO WRK-MODULO.

           CALL  WRK-MODULO             USING 63-ARGUMENTOS-ENTRADA
                                              63-RETORNO
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.

           IF  63-COD-RETORNO           NOT EQUAL ZEROS
               MOVE 63-COD-RETORNO      TO ROTSAI-COD-RETORNO
               MOVE 63-MENSAGEM         TO ROTSAI-MSG-RETORNO
               PERFORM 999-999-FINALIZAR
           END-IF.

           MOVE 63RT-DINIC-VGCIA-ISENC(1)
                                        TO WRK-CCHAVE-ELMTO-DESC-5.

      *---------------------------------------------------------------
       020-005-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-006-OBTER-63-ANTERIOR       SECTION.
      *---------------------------------------------------------------

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF.
4S2511     MOVE ROTENT-CCNPJ-CPF        TO 63-CCNPJ-CPF-ST.
4S2511*    MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ.
4S2511     MOVE ROTENT-CFLIAL-CNPJ      TO 63-CFLIAL-CNPJ-ST.
           MOVE ROTENT-CCTRL-CNPJ-CPF   TO 63-CCTRL-CNPJ-CPF.
           MOVE ROTENT-CTPO-ISENC-DESC  TO 63-CTPO-ISENC-DESC.
           MOVE ROTENT-CPRODT           TO 63-CPRODT.
           MOVE ROTENT-CSPROD-DESC-COML TO 63-CSPROD-DESC-COML.
           MOVE 'PARCIAL-06'            TO 63-INSTRUCAO.
           MOVE 'DCOM6815'              TO WRK-MODULO.

           CALL  WRK-MODULO             USING 63-ARGUMENTOS-ENTRADA
                                              63-RETORNO
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.

           IF  63-COD-RETORNO           NOT EQUAL ZEROS
               MOVE 63-COD-RETORNO      TO ROTSAI-COD-RETORNO
               MOVE 63-MENSAGEM         TO ROTSAI-MSG-RETORNO
               PERFORM 999-999-FINALIZAR
           END-IF.

           MOVE LENGTH                  OF 63-OCORRENCIAS(1)
                                        TO X1-WREG-LOG-ANTES-LEN.
           MOVE 63-OCORRENCIAS(1)       TO X1-WREG-LOG-ANTES-TEXT.

      *---------------------------------------------------------------
       020-006-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-010-VALIDAR-EXCLUSAO        SECTION.
      *---------------------------------------------------------------
      * VALIDA PSSOA CNPJ/CPF+TP.ISENC+ABRANGENCIA

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF       TO 63-CCNPJ-CPF.
4S2511     MOVE ROTENT-CCNPJ-CPF       TO 63-CCNPJ-CPF-ST.
4S2511*    MOVE ROTENT-CFLIAL-CNPJ     TO 63-CFLIAL-CNPJ.
4S2511     MOVE ROTENT-CFLIAL-CNPJ     TO 63-CFLIAL-CNPJ-ST.
           MOVE ROTENT-CCTRL-CNPJ-CPF  TO 63-CCTRL-CNPJ-CPF.
           MOVE ROTENT-CTPO-ISENC-DESC TO 63-CTPO-ISENC-DESC.
           MOVE ROTENT-DINIC-VGCIA-ISENC
                                       TO 63-DINIC-VGCIA-ISENC.
           MOVE ROTENT-CPRODT          TO 63-CPRODT.
           MOVE ROTENT-CSPROD-DESC-COML
                                       TO 63-CSPROD-DESC-COML.
           MOVE 'PARCIAL-01'           TO 63-INSTRUCAO.
           MOVE 'DCOM6815'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING 63-ARGUMENTOS-ENTRADA
                                             63-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.

           IF  63-COD-RETORNO          NOT EQUAL ZEROS AND 2
               IF  63-COD-RETORNO      EQUAL 3
                   MOVE '0664'         TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(664)
                                       TO ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'         TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(665)
                                       TO ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 999-999-FINALIZAR
           END-IF.

           MOVE 63RT-NSEQ-PSSOA-DESC(1)        TO  WRK-NSEQ-PSSOA-DESC.

           MOVE WRK-TIMESTAMP-ATUAL(1:4)       TO  WRK-DATA-ATU(1:4).
           MOVE WRK-TIMESTAMP-ATUAL(6:2)       TO  WRK-DATA-ATU(5:2).
           MOVE WRK-TIMESTAMP-ATUAL(9:2)       TO  WRK-DATA-ATU(7:2).

           MOVE ROTENT-DINIC-VGCIA-ISENC(1:2)  TO  WRK-DATA-INI(7:2).
           MOVE ROTENT-DINIC-VGCIA-ISENC(4:2)  TO  WRK-DATA-INI(5:2).
           MOVE ROTENT-DINIC-VGCIA-ISENC(7:4)  TO  WRK-DATA-INI(1:4).

           MOVE 63RT-DFIM-VGCIA-ISENC(1)       TO  WRK-DFIM-VGCIA-ISENC.

           MOVE WRK-DFIM-VGCIA-ISENC(1:2)      TO  WRK-DATA-FIM(7:2).
           MOVE WRK-DFIM-VGCIA-ISENC(4:2)      TO  WRK-DATA-FIM(5:2).
           MOVE WRK-DFIM-VGCIA-ISENC(7:4)      TO  WRK-DATA-FIM(1:4).

           IF  WRK-DATA-INI            LESS WRK-DATA-ATU
               IF  WRK-DATA-FIM        LESS WRK-DATA-ATU
                   MOVE '0666'         TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(666)
                                       TO ROTSAI-MSG-RETORNO
                   PERFORM 999-999-FINALIZAR
               ELSE
                   PERFORM 020-015-ENCERRAR-ISENCAO-SUBP
               END-IF
           ELSE
                PERFORM  020-020-EXCLUIR-ISENCAO-SUBP
           END-IF.

      *---------------------------------------------------------------
       020-010-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-015-ENCERRAR-ISENCAO-SUBP   SECTION.
      *---------------------------------------------------------------
      * ENCERRAR TABELA SUBPRODUTO-PSSOA-ISENCAO

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF          TO 63-CCNPJ-CPF.
4S2511     MOVE ROTENT-CCNPJ-CPF          TO 63-CCNPJ-CPF-ST.
           MOVE WRK-NSEQ-PSSOA-DESC       TO 63-NSEQ-PSSOA-DESC.
           MOVE ROTENT-CTPO-ISENC-DESC    TO 63-CTPO-ISENC-DESC.
           MOVE ROTENT-DINIC-VGCIA-ISENC  TO 63-DINIC-VGCIA-ISENC.
           MOVE ROTENT-CPRODT             TO 63-CPRODT.
           MOVE ROTENT-CSPROD-DESC-COML   TO 63-CSPROD-DESC-COML.
           MOVE WRK-DATA-ATU(1:4)         TO 63-DFIM-VGCIA-ISENC(7:4)
           MOVE '.'                       TO 63-DFIM-VGCIA-ISENC(6:1)
           MOVE WRK-DATA-ATU(5:2)         TO 63-DFIM-VGCIA-ISENC(4:2)
           MOVE '.'                       TO 63-DFIM-VGCIA-ISENC(3:1)
           MOVE WRK-DATA-ATU(7:2)         TO 63-DFIM-VGCIA-ISENC(1:2)
4S2511*    MOVE ROTENT-CFLIAL-CNPJ        TO 63-CFLIAL-CNPJ.
4S2511     MOVE ROTENT-CFLIAL-CNPJ        TO 63-CFLIAL-CNPJ-ST.
           MOVE ROTENT-CCTRL-CNPJ-CPF     TO 63-CCTRL-CNPJ-CPF.
           MOVE 63RT-CCLUB(1)             TO 63-CCLUB.
           MOVE 63RT-CINDCD-PSSOA-DESC(1) TO 63-CINDCD-PSSOA-DESC.
           MOVE 63RT-CINDCD-REST-VLR  (1) TO 63-CINDCD-REST-VLR.
           MOVE 63RT-VREST-MAX-OPER   (1) TO 63-VREST-MAX-OPER.
           MOVE ROTENT-CFUNC-BDSCO        TO 63-CFUNC-BDSCO.
           MOVE ROTENT-CTERM              TO 63-CTERM.
           MOVE 'TOTAL'                   TO 63-INSTRUCAO.
           MOVE 'DCOM6818'                TO WRK-MODULO

           CALL WRK-MODULO                USING 63-ARGUMENTOS-ENTRADA
                                                63-RETORNO
                                                LKG-ERRO-AREA
                                                LKG-SQLCA.

           IF  63-COD-RETORNO             NOT EQUAL ZEROS
               IF  63-COD-RETORNO         EQUAL 3
                   MOVE '0667'            TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(667)  TO ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'            TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(668)  TO ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 999-999-FINALIZAR
           END-IF.

           MOVE LENGTH                  OF 63-COLUNAS-DA-TABELA
                                        TO X1-WREG-LOG-APOS-LEN.
           MOVE 63-COLUNAS-DA-TABELA    TO X1-WREG-LOG-APOS-TEXT.

      *---------------------------------------------------------------
       020-015-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-020-EXCLUIR-ISENCAO-SUBP    SECTION.
      *---------------------------------------------------------------
      * EXCLUIR TABELA SUBPRODUTO-PSSOA-ISENCAO

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

           MOVE ROTENT-CCNPJ-CPF       TO 63-CCNPJ-CPF
           MOVE WRK-NSEQ-PSSOA-DESC    TO 63-NSEQ-PSSOA-DESC
           MOVE ROTENT-CTPO-ISENC-DESC TO 63-CTPO-ISENC-DESC
           MOVE ROTENT-DINIC-VGCIA-ISENC
                                       TO 63-DINIC-VGCIA-ISENC
           MOVE ROTENT-CPRODT          TO 63-CPRODT
           MOVE ROTENT-CSPROD-DESC-COML
                                       TO 63-CSPROD-DESC-COML
           MOVE 'DCOM6819'             TO WRK-MODULO

           CALL WRK-MODULO             USING 63-ARGUMENTOS-ENTRADA
                                             63-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.

           IF  63-COD-RETORNO          NOT EQUAL ZEROS
               IF  63-COD-RETORNO      EQUAL 3
                   MOVE '0669'         TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(669)
                                       TO ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'         TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(670)
                                       TO ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 999-999-FINALIZAR
           END-IF.

           MOVE ZEROS                  TO X1-WREG-LOG-APOS-LEN.
           MOVE SPACES                 TO X1-WREG-LOG-APOS-TEXT.

      *---------------------------------------------------------------
       020-020-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-030-GRAVAR-LOG-SUBP         SECTION.
      *---------------------------------------------------------------
      * GRAVAR  LOG TABELA ISENCAO-SUBPRODUTO

           MOVE 004                    TO X1-CELMTO-DESC-COML.

           IF ROTENT-CTPO-ISENC-DESC   EQUAL 1
              MOVE  058                TO X1-CEVNTO-LOG-DESC
           ELSE
              MOVE  061                TO X1-CEVNTO-LOG-DESC.

           MOVE 'E'                    TO X1-CINDCD-MANUT-REG.

           MOVE ROTENT-CPRODT          TO WRK-CCHAVE-ELMTO-DESC-1.
           MOVE ROTENT-CSPROD-DESC-COML
                                       TO WRK-CCHAVE-ELMTO-DESC-2.
           MOVE ROTENT-CCNPJ-CPF       TO WRK-CCHAVE-ELMTO-DESC-3.
           MOVE ROTENT-CTPO-ISENC-DESC TO WRK-CCHAVE-ELMTO-DESC-6.

           MOVE WRK-CCHAVE-ELMTO-DESC  TO X1-CCHAVE-ELMTO-DESC.
           MOVE 'DCOM5057'             TO X1-CTRANS-PROG-ATULZ.
           MOVE 'O'                    TO X1-CINDCD-MODLD-PROCM.
           MOVE 237                    TO X1-CEMPR-INC.
           MOVE 4130                   TO X1-CDEPDC.
           MOVE ROTENT-CFUNC-BDSCO     TO X1-CFUNC-BDSCO.
           MOVE ROTENT-CTERM           TO X1-CTERM.
           MOVE 'DCOM5075'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING X1-REGISTRO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.

           IF  X1-COD-RETORNO          NOT EQUAL ZEROS
               MOVE '0099'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(578)   TO ROTSAI-MSG-RETORNO
               PERFORM 999-999-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       020-030-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-040-VALIDAR-EXCLUSAO-PSSOA SECTION.
      *---------------------------------------------------------------

      * VALIDA EXCLUSAO PSSOA CNPJ/CPF+TP.ISENC+ABRANGENCIA

           INITIALIZE 63-ARGUMENTOS-ENTRADA
                      63-RETORNO.

           MOVE ROTENT-CCNPJ-CPF       TO 63-CCNPJ-CPF.
           MOVE ROTENT-CTPO-ISENC-DESC TO 63-CTPO-ISENC-DESC.
           MOVE ROTENT-DINIC-VGCIA-ISENC
                                       TO 63-DINIC-VGCIA-ISENC.
           MOVE ROTENT-CPRODT          TO 63-CPRODT.
           MOVE 'PARCIAL-12'           TO 63-INSTRUCAO.
           MOVE 'DCOM6816'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING 63-ARGUMENTOS-ENTRADA
                                             63-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.

           IF  63-COD-RETORNO          NOT EQUAL ZEROS AND 2 AND 3
               MOVE 63-COD-RETORNO     TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(658)   TO ROTSAI-MSG-RETORNO
               PERFORM 999-999-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       020-040-FIM.                    EXIT.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       020-050-EXCLUIR-PSSOA           SECTION.
      *---------------------------------------------------------------

      * EXCLUIR REGISTRO TABELA PSSOA-ISENCAO

4S2511*    MOVE ROTENT-CCNPJ-CPF       TO 48-CCNPJ-CPF
4S2511     MOVE ROTENT-CCNPJ-CPF       TO 48-CCNPJ-CPF-ST
           MOVE WRK-NSEQ-PSSOA-DESC    TO 48-NSEQ-PSSOA-DESC
           MOVE ROTENT-DINIC-VGCIA-ISENC
                                       TO 48-DINIC-VGCIA-ISENC
           MOVE ROTENT-CTPO-ISENC-DESC TO 48-CTPO-ISENC-DESC
           MOVE 'DCOM6744'             TO WRK-MODULO

           CALL WRK-MODULO             USING 48-ARGUMENTOS-ENTRADA
                                             48-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.

           IF  48-COD-RETORNO          NOT EQUAL ZEROS
               IF  48-COD-RETORNO      EQUAL 003
                   MOVE '0671'         TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(671)
                                       TO ROTSAI-MSG-RETORNO
               ELSE
                   MOVE 48-COD-RETORNO TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(672)
                                       TO ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 999-999-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       020-050-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       999-999-FINALIZAR                    SECTION.
      *---------------------------------------------------------------
      * PROCEDIMENTOS FINAIS

           GOBACK.

      *---------------------------------------------------------------
       015-000-FIM.
      *---------------------------------------------------------------

