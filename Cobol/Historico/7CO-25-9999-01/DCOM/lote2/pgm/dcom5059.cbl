      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*

       PROGRAM-ID.                     DCOM5059.
       AUTHOR.                         VITAL.

      *----------------------------------------------------------------*
      *                       ALTRAN CONSULTORIA                       *
      *----------------------------------------------------------------*
      *                                                                *
      *     PROGRAMA     : DCOM5059                                    *
      *     PROGRAMADOR  : VITAL                     -   ALTRAN        *
      *     ANALISTA     : MAURICIO FRANCO DA SILVA  -   ALTRAN        *
      *     DATA         : MAIO/2006                                   *
      *                                                                *
      *     OBJETIVO     : INCLUSAO ISENCAO DA ANALISE DE RESTRICAO DO *
      *                    SACADO POR CEDENTE NO GERAL.                *
      *                                                                *
      *     INC'S        :                                             *
      *       I#DCOMSG   - AREA DE RETORNO DAS MENSAGENS DE ERRO       *
      *       I#DCOM48   - TABELA TPSSOA_ISENC_DESC                    *
      *       I#DCOM56   - TABELA TSACDO_ISENC_CDENT                   *
      *       I#DCOMX1   - ATUALIZA LOG                                *
      *       I#BRAD7C   - AREA PARA TRATAMENTO DE ERROS DB2           *
      *                                                                *
      *     MODULOS      :                                             *
      *       DCOM5022   - ACESSO AOS DADOS DA TABELA DCOMB020 (SELECT)*
      *       DCOM6408   - ACESSO AOS DADOS DO MODULO DO - CADU        *
      *       DCOM6741   - TP DE ISENCAO SOLICITADA DCOMB048 (FETCH)   *
      *       DCOM6742   - ATUALIZACAO DA TABELA DCOMB048 (INSERT)     *
      *       DCOM6782   - ATUALIZACAO DA TABELA DCOMB056 (INSERT)     *
      *       DCOM5075   - ATUALIZAR LOG DA INCLUSAO REALIZADA         *
      *       POOL7100   - TRATAMENTO DE ERROS                         *
      *                                                                *
      *  PRINCIPAIS IMPLEMENTACOES-                                    *
      *  A - VALIDACAO DE INCLUSAO DE MESMO CPF                        *
      *  B - AJUSTE NA OBTENCAO DA NUMERACAO DO DCOM                   *
      *  C - EXCLUSAO DA COLUNA PRODUTO NA TABELA TPSSOA_ISENC_DESC    *
      *                                                                *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)        VALUE
           '*** INICIO WORKING-STORAGE DCOM5059 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)        VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(009) COMP-3  VALUE ZEROS.

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-FIM-LISTA               PIC  X(001)         VALUE SPACES.
       77  WRK-ERRO                    PIC  9(003)         VALUE ZEROS.
       77  WRK-NSEQ-PSSOA-DESC         PIC  9(003)         VALUE ZEROS.
       77  WRK-TIMESTAMP-ATUAL         PIC  X(026)         VALUE SPACES.
4S2511* 77  WRK-CCNPJ-CPF               PIC  9(009)         VALUE ZEROS.
4S2511 77  WRK-CCNPJ-CPF               PIC  X(009)         VALUE SPACES.
       77  WRK-CTPO-ISENC-DESC         PIC  9(003)         VALUE ZEROS.
4S2511* 77  WRK-CFLIAL-CNPJ             PIC  9(005)         VALUE ZEROS.
4S2511 77  WRK-CFLIAL-CNPJ             PIC  X(004)         VALUE SPACES.
       77  WRK-CCTRL-CNPJ-CPF          PIC  9(002)         VALUE ZEROS.
       77  WRK-DATA-1                  PIC  X(010)         VALUE SPACES.
       77  WRK-DATA-2                  PIC  X(010)         VALUE SPACES.

       01  WRK-CCHAVE-ELMTO-DESC.
4S2511*    05  WRK-CCHAVE-ELMTO-DESC-1 PIC  9(009)         VALUE ZEROS.
4S2511     05  WRK-CCHAVE-ELMTO-DESC-1 PIC  X(009)         VALUE SPACES.
           05  WRK-CCHAVE-ELMTO-DESC-2 PIC  X(010)         VALUE SPACES.
           05  WRK-CCHAVE-ELMTO-DESC-3 PIC  9(003)         VALUE ZEROS.
           05  WRK-CCHAVE-ELMTO-DESC-4 PIC  X(023)         VALUE SPACES.

       01  WRK-DT-INI-ENT              PIC  9(008)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-DT-INI-ENT.
           05  WRK-AINI-ENT            PIC  X(004).
           05  WRK-MINI-ENT            PIC  X(002).
           05  WRK-DINI-ENT            PIC  X(002).

       01  WRK-DT-FIM-ENT              PIC  9(008)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-DT-FIM-ENT.
           05  WRK-AFIM-ENT            PIC  X(004).
           05  WRK-MFIM-ENT            PIC  X(002).
           05  WRK-DFIM-ENT            PIC  X(002).

       01  WRK-DT-INI-TAB              PIC  9(008)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-DT-INI-TAB.
           05  WRK-AINI-TAB            PIC  X(004).
           05  WRK-MINI-TAB            PIC  X(002).
           05  WRK-DINI-TAB            PIC  X(002).

       01  WRK-DT-FIM-TAB              PIC  9(008)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-DT-FIM-TAB.
           05  WRK-AFIM-TAB            PIC  X(004).
           05  WRK-MFIM-TAB            PIC  X(002).
           05  WRK-DFIM-TAB            PIC  X(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM6741 *'.
      *       DCOM6741   - TP DE ISENCAO SOLICITADA DCOMB048 (FETCH)   *
      *----------------------------------------------------------------*

       COPY 'I#DCOM48'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM6782 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM56'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM5075 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMX1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(029)        VALUE
           '* AREA DE MENSAGENS DO DCOM *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSG'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(020)        VALUE
           '* AREA DA I#BRAD7C *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(039)        VALUE
           '*** FINAL WORKING-STORAGE DCOM5059 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR.
           03  ROTENT-COD-RETORNO      PIC  X(004).
           03  ROTENT-MSG-RETORNO      PIC  X(079).
           03  ROTENT-RESTART          PIC  9(005).
           03  ROTENT-FLAG             PIC  X(001).
           03  ROTENT-CFUNC-BDSCO      PIC  9(009).
           03  ROTENT-CTERM            PIC  X(008).
           03  ROTENT-CTPO-ISENC-DESC  PIC  9(003).
4S2511*      03  ROTENT-CCNPJ-CPF        PIC  9(009).
4S2511       03  ROTENT-CCNPJ-CPF        PIC  X(009).
4S2511*      03  ROTENT-CFLIAL-CNPJ      PIC  9(005).
4S2511       03  ROTENT-CFLIAL-CNPJ      PIC  X(004).
           03  ROTENT-CCTRL-CNPJ-CPF   PIC  9(002).
           03  ROTENT-DINIC-VGCIA-ISENC
                                       PIC  X(010).
           03  ROTENT-DFIM-VGCIA-ISENC PIC  X(010).
           03  ROTENT-CINDCD-REST-CDENT
                                       PIC  X(001).
4S2511*    03  ROTENT-CCNPJ-CPF-PROPN  PIC  9(009).
4S2511     03  ROTENT-CCNPJ-CPF-PROPN  PIC  X(009).
4S2511*    03  ROTENT-CFLIAL-CNPJ-PROPN
4S2511*                                PIC  9(005).
4S2511     03  ROTENT-CFLIAL-CNPJ-PROPN
4S2511                                 PIC  X(004).
           03  ROTENT-CCTRL-CNPJ-PROPN PIC  9(002).

       01  SAIDA-ROTEADOR.
           03  ROTSAI-HEADER.
               05  ROTSAI-COD-RETORNO  PIC  X(004).
               05  ROTSAI-MSG-RETORNO  PIC  X(079).
               05  ROTSAI-RESTART      PIC  9(005).
               05  ROTSAI-FLAG         PIC  X(001).
           03  ROTSAI-CONSISTENCIA.
               05  ROTSAI-TAMANHO-ERRO PIC  9(001).
               05  ROTSAI-TABELA-ERROS OCCURS  006  TIMES.
                   07  ROTSAI-CAMPOS-ERRO
                                       PIC  9(001).

       01  LKG-ERRO-AREA               PIC  X(107).

       01  LKG-SQLCA                   PIC  X(136).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION           USING  ENTRADA-ROTEADOR
                                           SAIDA-ROTEADOR
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       00000-PRINCIPAL                 SECTION.
      *----------------------------------------------------------------*

           PERFORM  10000-INICIALIZAR.

           PERFORM  20000-CONSISTIR-ENTRADA.

           PERFORM  30000-PROCESSAR.

           PERFORM  50000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  SAIDA-ROTEADOR
                       48-ARGUMENTOS-ENTRADA
                       56-ARGUMENTOS-ENTRADA
                       X1-ARGUMENTOS-ENTRADA
                       LKG-ERRO-AREA
                       LKG-SQLCA
                       ERRO-AREA.

           MOVE  1                        TO  ROTSAI-TAMANHO-ERRO.
           MOVE  '0000'                   TO  ROTSAI-COD-RETORNO.
           MOVE  SG-MENSAGEM(001)         TO  ROTSAI-MSG-RETORNO.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-CONSISTIR-ENTRADA         SECTION.
      *----------------------------------------------------------------*

           IF  ROTENT-CTPO-ISENC-DESC   NOT NUMERIC OR
               ROTENT-CTPO-ISENC-DESC   NOT EQUAL  1
               MOVE  1                  TO ROTSAI-CAMPOS-ERRO(1)
               MOVE '0495'              TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(495)    TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

4S2511*    IF  ROTENT-CCNPJ-CPF         EQUAL ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF         EQUAL SPACES OR
4S2511                                 LOW-VALUES
               MOVE  1                  TO ROTSAI-CAMPOS-ERRO(2)
               MOVE '0223'              TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(223)    TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

           IF  ROTENT-DINIC-VGCIA-ISENC EQUAL SPACES
               MOVE  1                  TO ROTSAI-CAMPOS-ERRO(3)
               MOVE '0540'              TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(540)    TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

           IF  ROTENT-DFIM-VGCIA-ISENC  EQUAL SPACES
               MOVE  1                  TO ROTSAI-CAMPOS-ERRO(4)
               MOVE '0544'              TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(544)    TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

           IF  ROTENT-CINDCD-REST-CDENT NOT EQUAL 'S' AND 'N'
               MOVE  1                  TO ROTSAI-CAMPOS-ERRO(5)
               MOVE '0860'              TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(860)    TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

           IF  ROTENT-CINDCD-REST-CDENT EQUAL 'S' AND
4S2511*        ROTENT-CCNPJ-CPF-PROPN   EQUAL  ZEROS
4S2511         ROTENT-CCNPJ-CPF-PROPN   EQUAL  SPACES OR
4S2511                                 LOW-VALUES
               MOVE  1                  TO ROTSAI-CAMPOS-ERRO(5)
               MOVE '0549'              TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(549)    TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 31000-OBTEM-TIMESTAMP.

           IF  ROTENT-CINDCD-REST-CDENT EQUAL 'N'
               PERFORM 32000-VALIDA-PESSOA
           ELSE
               PERFORM 33000-VALIDA-PROPONENTE
           END-IF.

           PERFORM 34000-OBTER-SEQUENCIA.

           PERFORM 35000-GRAVAR-ISENCOES.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       31000-OBTEM-TIMESTAMP           SECTION.
      *----------------------------------------------------------------*

           MOVE  'DCOM7999'   TO  WRK-MODULO.

           CALL  WRK-MODULO   USING  WRK-TIMESTAMP-ATUAL
                                     LKG-ERRO-AREA
                                     LKG-SQLCA.

      *----------------------------------------------------------------*
       31000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32000-VALIDA-PESSOA             SECTION.
      *----------------------------------------------------------------*

           MOVE  'PARCIAL-81'             TO 48-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC TO 48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC  TO 48-DFIM-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 48-CTPO-ISENC-DESC.

4S2511*    IF ROTENT-CCNPJ-CPF-PROPN      EQUAL ZEROS
4S2511     IF ROTENT-CCNPJ-CPF-PROPN      EQUAL SPACES OR
4S2511                                 LOW-VALUES
              MOVE 005                    TO 48-CABRGC-ISENC-DESC
           ELSE
              MOVE 006                    TO 48-CABRGC-ISENC-DESC
           END-IF.

           MOVE  'DCOM6741'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING
                                       48-ARGUMENTOS-ENTRADA
                                       48-RETORNO
                                       LKG-ERRO-AREA
                                       LKG-SQLCA
                                       ROTENT-DINIC-VGCIA-ISENC
                                       ROTENT-DFIM-VGCIA-ISENC.

           IF  48-COD-RETORNO          EQUAL ZEROS OR 2 OR 3
               IF  48-QTDE-TOTAL       GREATER ZEROS
                   MOVE '0768'         TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(768)
                                       TO ROTSAI-MSG-RETORNO
                   PERFORM 50000-FINALIZAR
               END-IF
           ELSE
               MOVE '0099'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(099)   TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       32000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       33000-VALIDA-PROPONENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE  'PARCIAL-CT'             TO 56-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 56-CCNPJ-CPF.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ       TO 56-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 56-CCNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ       TO 56-CFLIAL-CNPJ-ST.
4S2511*    MOVE  ROTENT-CCNPJ-CPF-PROPN   TO 56-CCNPJ-CPF-PROPN.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ-PROPN TO 56-CFLIAL-CNPJ-PROPN.
4S2511     MOVE  ROTENT-CCNPJ-CPF-PROPN   TO 56-CCNPJ-CPF-PROPN-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ-PROPN TO 56-CFLIAL-CNPJ-PROPN-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC TO 56-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC  TO 56-DFIM-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 56-CTPO-ISENC-DESC.

           MOVE  'DCOM6780'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING
                                       56-ARGUMENTOS-ENTRADA
                                       56-RETORNO
                                       LKG-ERRO-AREA
                                       LKG-SQLCA.

           IF 56-COD-RETORNO EQUAL ZEROS
              IF 56-QTDE-TOTAL GREATER   ZEROS
                 MOVE   '0768'      TO  ROTSAI-COD-RETORNO
                 MOVE   SG-MENSAGEM(768)
                                    TO  ROTSAI-MSG-RETORNO
                 PERFORM  50000-FINALIZAR
              END-IF
           ELSE
              IF 56-COD-RETORNO NOT EQUAL 3
                 MOVE   '0099'         TO  ROTSAI-COD-RETORNO
                 MOVE   SG-MENSAGEM(099)
                                       TO  ROTSAI-MSG-RETORNO
                 PERFORM  50000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       33000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       34000-OBTER-SEQUENCIA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 48-ARGUMENTOS-ENTRADA
                      48-RETORNO

           MOVE ZEROS                  TO WRK-NSEQ-PSSOA-DESC.
4S2511*    MOVE ROTENT-CCNPJ-CPF       TO 48-CCNPJ-CPF.
4S2511     MOVE ROTENT-CCNPJ-CPF       TO 48-CCNPJ-CPF-ST.
           MOVE 'PARCIAL-07'           TO 48-INSTRUCAO.
           MOVE 'DCOM6740'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 48-ARGUMENTOS-ENTRADA
                                             48-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.

           IF  48-COD-RETORNO          NOT EQUAL ZEROS AND 003
               MOVE 48-COD-RETORNO     TO ROTSAI-COD-RETORNO
               MOVE 48-MENSAGEM        TO ROTSAI-MSG-RETORNO
               MOVE 'ERRO DE ACESSO AO 6740 - PARCIAL-07'
                                       TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

           IF  48-COD-RETORNO          EQUAL ZEROS
               COMPUTE WRK-NSEQ-PSSOA-DESC
                                       = 48RT-NSEQ-PSSOA-DESC(1) + 1
           ELSE
               MOVE 1                  TO WRK-NSEQ-PSSOA-DESC
           END-IF.

      *----------------------------------------------------------------*
       34000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       35000-GRAVAR-ISENCOES           SECTION.
      *----------------------------------------------------------------*

           IF  ROTENT-CINDCD-REST-CDENT EQUAL 'N'
               MOVE 5                   TO 48-CABRGC-ISENC-DESC
               PERFORM 35100-GRAVAR-ISENCAO-DESC-COML
           ELSE
               MOVE 6                   TO 48-CABRGC-ISENC-DESC
               PERFORM 35100-GRAVAR-ISENCAO-DESC-COML
               PERFORM 35200-GRAVAR-SACADO-ISEN-CEDEN
           END-IF.

           PERFORM  35300-GRAVAR-LOG.

      *----------------------------------------------------------------*
       35000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       35100-GRAVAR-ISENCAO-DESC-COML  SECTION.
      *----------------------------------------------------------------*

           PERFORM 35050-OBTER-DT-MIN.

           INITIALIZE 48-ARGUMENTOS-ENTRADA
                      48-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF       TO 48-CCNPJ-CPF.
4S2511     MOVE ROTENT-CCNPJ-CPF       TO 48-CCNPJ-CPF-ST.
           MOVE WRK-NSEQ-PSSOA-DESC    TO 48-NSEQ-PSSOA-DESC.
           MOVE ROTENT-DINIC-VGCIA-ISENC
                                       TO 48-DINIC-VGCIA-ISENC.
           MOVE ROTENT-CTPO-ISENC-DESC TO 48-CTPO-ISENC-DESC.
           MOVE ROTENT-DFIM-VGCIA-ISENC
                                       TO 48-DFIM-VGCIA-ISENC.
4S2511*    MOVE ROTENT-CFLIAL-CNPJ     TO 48-CFLIAL-CNPJ.
4S2511     MOVE ROTENT-CFLIAL-CNPJ     TO 48-CFLIAL-CNPJ-ST.
4S2511*    MOVE ROTENT-CCTRL-CNPJ-CPF  TO 48-CCTRL-CNPJ-CPF.
4S2511     MOVE ROTENT-CCTRL-CNPJ-CPF  TO 48-CCTRL-CNPJ-CPF-ST.
           MOVE 1                      TO 48-CCLUB.

4S2511*    IF  ROTENT-CFLIAL-CNPJ      EQUAL ZEROS
4S2511     IF  ROTENT-CFLIAL-CNPJ      EQUAL SPACES OR
4S2511                                 LOW-VALUES
               MOVE  'F'               TO 48-CINDCD-PSSOA-DESC
           ELSE
               MOVE  'J'               TO 48-CINDCD-PSSOA-DESC
           END-IF.

4S2511*    IF  ROTENT-CCNPJ-CPF-PROPN  EQUAL ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF-PROPN  EQUAL SPACES OR
4S2511                                 LOW-VALUES
               MOVE 005                TO 48-CABRGC-ISENC-DESC
           ELSE
               MOVE 006                TO 48-CABRGC-ISENC-DESC
           END-IF.

           MOVE ROTENT-CINDCD-REST-CDENT
                                       TO 48-CINDCD-REST-CDENT.
           MOVE ROTENT-CFUNC-BDSCO     TO 48-CFUNC-BDSCO.
           MOVE ROTENT-CTERM           TO 48-CTERM.
           MOVE 'DCOM6742'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 48-ARGUMENTOS-ENTRADA
                                             48-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA
                                             ROTENT-DINIC-VGCIA-ISENC
                                             ROTENT-DFIM-VGCIA-ISENC.

           IF  48-COD-RETORNO          NOT EQUAL ZEROS AND 4
               MOVE '0099'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(861)   TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

           MOVE LENGTH                 OF 48-COLUNAS-DA-TABELA
                                       TO X1-WREG-LOG-APOS-LEN.
           MOVE 48-COLUNAS-DA-TABELA   TO X1-WREG-LOG-APOS-TEXT.

      *----------------------------------------------------------------*
       35100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       35050-OBTER-DT-MIN              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 48-ARGUMENTOS-ENTRADA
                      48-RETORNO.

4S2511*    MOVE ROTENT-CCNPJ-CPF       TO 48-CCNPJ-CPF.
4S2511     MOVE ROTENT-CCNPJ-CPF       TO 48-CCNPJ-CPF-ST.
           MOVE ROTENT-CTPO-ISENC-DESC TO 48-CTPO-ISENC-DESC.
           MOVE 'PARCIAL-20'           TO 48-INSTRUCAO.
           MOVE 'DCOM6741'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING 48-ARGUMENTOS-ENTRADA
                                             48-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA
                                             ROTENT-DINIC-VGCIA-ISENC
                                             ROTENT-DFIM-VGCIA-ISENC.

           IF  48-COD-RETORNO          NOT EQUAL ZEROS AND 002 AND 003
               MOVE 48-COD-RETORNO     TO ROTSAI-COD-RETORNO
               MOVE 48-MENSAGEM        TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

           IF  48-COD-RETORNO          EQUAL ZEROS OR 002
               MOVE 48RT-DINIC-VGCIA-ISENC(1)
                                       TO WRK-CCHAVE-ELMTO-DESC-2
               MOVE LENGTH             OF 48-OCORRENCIAS(1)
                                       TO X1-WREG-LOG-ANTES-LEN
               MOVE 48-OCORRENCIAS(1)  TO X1-WREG-LOG-ANTES-TEXT
               MOVE 'A'                TO X1-CINDCD-MANUT-REG
           ELSE
               MOVE ROTENT-DINIC-VGCIA-ISENC
                                       TO WRK-CCHAVE-ELMTO-DESC-2
               MOVE ZEROS              TO X1-WREG-LOG-ANTES-LEN
               MOVE SPACES             TO X1-WREG-LOG-ANTES-TEXT
               MOVE 'I'                TO X1-CINDCD-MANUT-REG
           END-IF.

      *----------------------------------------------------------------*
       35050-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       35200-GRAVAR-SACADO-ISEN-CEDEN  SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF      TO  56-CCNPJ-CPF.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ    TO  56-CFLIAL-CNPJ.
4S2511*    MOVE  ROTENT-CCTRL-CNPJ-CPF TO  56-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF      TO  56-CCNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ    TO  56-CFLIAL-CNPJ-ST.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF TO  56-CCTRL-CNPJ-CPF-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC
                                       TO  56-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC
                                       TO  56-DFIM-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC
                                       TO  56-CTPO-ISENC-DESC.
           MOVE  WRK-NSEQ-PSSOA-DESC   TO  56-NSEQ-PSSOA-DESC.
           MOVE  1                     TO  56-CCLUB-CDENT-PROPN.
4S2511*    MOVE  ROTENT-CCNPJ-CPF-PROPN
4S2511*                                TO  56-CCNPJ-CPF-PROPN.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ-PROPN
4S2511*                                TO  56-CFLIAL-CNPJ-PROPN.
4S2511*    MOVE  ROTENT-CCTRL-CNPJ-PROPN
4S2511*                                TO  56-CCTRL-CNPJ-PROPN.
4S2511     MOVE  ROTENT-CCNPJ-CPF-PROPN
4S2511                                 TO  56-CCNPJ-CPF-PROPN-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ-PROPN
4S2511                                 TO  56-CFLIAL-CNPJ-PROPN-ST.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-PROPN
4S2511                                 TO  56-CCTRL-CNPJ-PROPN-ST.
           MOVE  ROTENT-CFUNC-BDSCO    TO  56-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM          TO  56-CTERM.
           MOVE  'DCOM6782'            TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  56-ARGUMENTOS-ENTRADA
                                           56-RETORNO
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  56-COD-RETORNO               NOT EQUAL ZEROS
               IF  56-COD-RETORNO           EQUAL 4
                   MOVE '0862'              TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(862)    TO ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'              TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(863)    TO ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 50000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       35200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       35300-GRAVAR-LOG                SECTION.
      *----------------------------------------------------------------*

           MOVE 002                    TO X1-CELMTO-DESC-COML.
           MOVE 059                    TO X1-CEVNTO-LOG-DESC.

           MOVE ROTENT-CCNPJ-CPF       TO WRK-CCHAVE-ELMTO-DESC-1.
           MOVE ROTENT-CTPO-ISENC-DESC TO WRK-CCHAVE-ELMTO-DESC-3.
           MOVE SPACES                 TO WRK-CCHAVE-ELMTO-DESC-4.
           MOVE WRK-CCHAVE-ELMTO-DESC  TO X1-CCHAVE-ELMTO-DESC.

           MOVE 'DCOM5059'             TO X1-CTRANS-PROG-ATULZ.
           MOVE 'O'                    TO X1-CINDCD-MODLD-PROCM.
           MOVE 237                    TO X1-CEMPR-INC.
           MOVE 4130                   TO X1-CDEPDC.
           MOVE ROTENT-CFUNC-BDSCO     TO X1-CFUNC-BDSCO.
           MOVE ROTENT-CTERM           TO X1-CTERM.
           MOVE 'DCOM5075'             TO WRK-MODULO.

           CALL WRK-MODULO             USING X1-REGISTRO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.

           IF  X1-COD-RETORNO          NOT EQUAL ZEROS
               MOVE X1-COD-RETORNO     TO ROTSAI-COD-RETORNO
               MOVE X1-MENSAGEM        TO ROTSAI-MSG-RETORNO
               PERFORM 50000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       35300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       50000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           IF  ROTSAI-COD-RETORNO NOT EQUAL '0000' AND '0002' AND
                                            '0003' AND '0004' AND
                                            '0005'
               NEXT SENTENCE
           ELSE
               MOVE  'DCOM5059'    TO  ERR-PGM
               MOVE  SPACES        TO  LKG-SQLCA
               MOVE  ERRO-AREA     TO  LKG-ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       50000-FIM.                      EXIT.
      *----------------------------------------------------------------*
