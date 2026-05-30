       IDENTIFICATION                       DIVISION.
       PROGRAM-ID.                          DCOM5054.
       AUTHOR.                              FERRARINI.
      *DATE-WRITTEN.                        26-04-2006.
      *DATE-COMPILLED.
      ******************************************************************
      * INCLUIR ISENCAO OPERACAO POR CEDENTE
      ******************************************************************
       ENVIRONMENT                          DIVISION.
       CONFIGURATION                        SECTION.
       SPECIAL-NAMES.                       DECIMAL-POINT IS COMMA.
       DATA                                 DIVISION.
       WORKING-STORAGE                      SECTION.
       01            FILLER                 PIC  X(032)
                     VALUE '*** INICIO WORKING DCOM5054 ***'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOMSG           ***'.

           COPY      'I#DCOMSG'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOM48           ***'.

           COPY      'I#DCOM48'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOMG9           ***'.

           COPY      'I#DCOMG9'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOM36           ***'.

           COPY      'I#DCOM36'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOMG4 - DCOM7320 **'.

           COPY      'I#DCOMG4'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOM12           ***'.

           COPY      'I#DCOM12'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOMX1           ***'.

           COPY      'I#DCOMX1'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#BRAD7C           ***'.

           COPY      'I#BRAD7C'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** AUXILIARES              ***'.

       01            WS-AUX.
           03        WRK-ERRO          PIC  9(003)
                     VALUE ZEROS.
           03        WRK-MODULO        PIC  X(008)
                     VALUE SPACES.
           03        WRK-TEM-PESSOA    PIC  X(001)
                     VALUE 'N'.
           03        IND-LKG           PIC  9(002)
                     VALUE ZEROS.
           03        IND-ENT           PIC  9(002)
                     VALUE ZEROS.

       01  WRK-TRATA-ALTERACAO         PIC  X(001) VALUE SPACES.
       01  WRK-TIMESTAMP-ATUAL         PIC  X(026) VALUE SPACES.
       01  WRK-TIMESTAMP-ATUAL-D       PIC  X(026) VALUE 'DATA'.

       01  WRK-ENTRADA.
           03  WRK-COD-RETORNO-E       PIC  X(004).
           03  WRK-MSG-RETORNO-E       PIC  X(079).
           03  WRK-RESTART-E           PIC  9(005).
           03  WRK-FLAG-E              PIC  X(001).
           03  WRK-CFUNC-BDSCO-E       PIC  9(009).
           03  WRK-CTERM-E             PIC  X(008).
           03  WRK-DADOS-E.
               05  WRK-CELMTO-DESC-COML-E     PIC  9(03).
               05  WRK-WCHAVE-PRINC-ELMTO-E   PIC  9(09).
               05  WRK-QDIG-SEQ-ELMTO-E       PIC  9(03).

       01  WRK-SAIDA.
           03  WRK-HEADER-S.
               05  WRK-COD-RETORNO-S          PIC  X(04).
               05  WRK-MSG-RETORNO-S          PIC  X(79).
               05  WRK-RESTART-S              PIC  9(05).
               05  WRK-FLAG-S                 PIC  X(01).
               05  WRK-NULT-SEQ-ELMTO-S       PIC  9(15).


       LINKAGE                              SECTION.

       01        ENTRADA-ROTEADOR.
           03    ROTENT-COD-RETORNO              PIC X(04).
           03    ROTENT-MSG-RETORNO              PIC X(79).
           03    ROTENT-RESTART                  PIC 9(05).
           03    ROTENT-FLAG                     PIC X(01).
           03    ROTENT-CFUNC-BDSCO              PIC 9(09).
           03    ROTENT-CTERM                    PIC X(08).
4S2511*    03    ROTENT-CCNPJ-CPF                PIC 9(09).
4S2511*    03    ROTENT-CFLIAL-CNPJ              PIC 9(04).
4S2511     03    ROTENT-CCNPJ-CPF                PIC X(09).
4S2511     03    ROTENT-CFLIAL-CNPJ              PIC X(04).
           03    ROTENT-CCTRL-CNPJ-CPF           PIC 9(02).
           03    TABELA-OPERACOES OCCURS 70 TIMES.
             05  ROTENT-DANO-OPER-DESC           PIC 9(04).
             05  ROTENT-NSEQ-OPER-DESC           PIC 9(09).
             05  ROTENT-CAG-BCRIA                PIC 9(05).
             05  ROTENT-CCTA-BCRIA-CLI           PIC 9(13).

       01        SAIDA-ROTEADOR.
           03    ROTSAI-HEADER.
            05   ROTSAI-COD-RETORNO              PIC X(04).
            05   ROTSAI-MSG-RETORNO              PIC X(79).
            05   ROTSAI-RESTART                  PIC 9(05).
            05   ROTSAI-FLAG                     PIC X(01).

           03    ROTSAI-CONSISTENCIA.
            05   ROTSAI-TAMANHO-ERRO             PIC 9(02).
            05   ROTSAI-TABELA-ERROS             OCCURS 70 TIMES.
             07  ROTSAI-CAMPOS-ERRO              PIC 9(01).

       01           LKG-ERRO-AREA           PIC  X(107).
       01           LKG-SQLCA               PIC  X(136).

       PROCEDURE                            DIVISION
                                            USING  ENTRADA-ROTEADOR,
                                                   SAIDA-ROTEADOR,
                                                   LKG-ERRO-AREA,
                                                   LKG-SQLCA.

       000-000-INICIO                       SECTION.

           INITIALIZE SAIDA-ROTEADOR
                      36-ARGUMENTOS-ENTRADA
                      36-RETORNO
                      48-ARGUMENTOS-ENTRADA
                      48-RETORNO
                      G9-ARGUMENTOS-ENTRADA
                      G9-RETORNO
                      X1-ARGUMENTOS-ENTRADA
                      LKG-ERRO-AREA
                      LKG-SQLCA.

           MOVE   2                   TO ROTSAI-TAMANHO-ERRO.
           MOVE  '0000'               TO  ROTSAI-COD-RETORNO.
           MOVE  SG-MENSAGEM(001)     TO  ROTSAI-MSG-RETORNO.

           PERFORM 020-010-OBTER-TIMESTAMP.

           PERFORM   010-000-CONSISTIR-ENTRADAS.

           PERFORM   020-000-TRATAR-NEGOCIO.

           PERFORM   030-000-FINALIZAR.

       000-000-FIM.

      ******************************************************************
       010-000-CONSISTIR-ENTRADAS         SECTION.
      ******************************************************************


4S2511*    IF  ROTENT-CCNPJ-CPF         NOT NUMERIC
4S2511*    OR  ROTENT-CCNPJ-CPF         EQUAL ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF         NOT EQUAL SPACES
4S2511     OR  ROTENT-CCNPJ-CPF         NOT EQUAL LOW-VALUES
               ADD   1                  TO WRK-ERRO
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
               MOVE  0223               TO ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(0223)  TO ROTSAI-MSG-RETORNO
               MOVE  'APL'              TO ERR-TIPO-ACESSO
               MOVE  ROTSAI-MSG-RETORNO TO ERR-TEXTO
               PERFORM 030-000-FINALIZAR
           END-IF.

           IF  ROTENT-DANO-OPER-DESC(1) EQUAL ZEROS
               ADD   1                  TO WRK-ERRO
               MOVE  2                 TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
               MOVE  0117               TO ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(0117)  TO ROTSAI-MSG-RETORNO
               MOVE  'APL'              TO ERR-TIPO-ACESSO
               MOVE  ROTSAI-MSG-RETORNO TO ERR-TEXTO
               PERFORM 030-000-FINALIZAR
           END-IF.

           PERFORM VARYING IND-ENT FROM 1 BY 1
                   UNTIL IND-ENT GREATER 70
             IF  ROTENT-DANO-OPER-DESC(IND-ENT) NOT NUMERIC
                ADD   1                  TO WRK-ERRO
                MOVE  3                 TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
                MOVE  0117               TO ROTSAI-COD-RETORNO
                MOVE  SG-MENSAGEM(0117)  TO ROTSAI-MSG-RETORNO
                MOVE  'APL'              TO ERR-TIPO-ACESSO
                MOVE  ROTSAI-MSG-RETORNO TO ERR-TEXTO
                PERFORM 030-000-FINALIZAR
             END-IF
           END-PERFORM.

           PERFORM VARYING IND-ENT FROM 1 BY 1
                   UNTIL IND-ENT GREATER 70
                   OR    ROTENT-DANO-OPER-DESC(IND-ENT) EQUAL ZEROS
                   PERFORM 010-010-VALIDA-OPERACAO
           END-PERFORM.

       010-000-FIM.

      ******************************************************************
       010-010-VALIDA-OPERACAO            SECTION.
      ******************************************************************

      * INCLUIR TABELA OPERACAO-PESSOA-ISENCAO

4S2511*    MOVE  ROTENT-CCNPJ-CPF               TO 36-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF               TO 36-CCNPJ-CPF-ST.
WPG        MOVE  WRK-TIMESTAMP-ATUAL-D          TO 36-DINIC-VGCIA-ISENC
                                                   36-DFIM-VGCIA-ISENC.
           MOVE  1                              TO 36-CTPO-ISENC-DESC.
           MOVE  ROTENT-DANO-OPER-DESC(IND-ENT) TO 36-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC(IND-ENT) TO 36-NSEQ-OPER-DESC.


           MOVE  'PARCIAL-01'                   TO 36-INSTRUCAO.
           MOVE  'DCOM6680'                     TO WRK-MODULO.

           CALL      WRK-MODULO          USING  36-ARGUMENTOS-ENTRADA
                                                36-RETORNO
                                                LKG-ERRO-AREA
                                                LKG-SQLCA.

           IF  36-COD-RETORNO NOT EQUAL ZEROS AND 3 AND 4
               MOVE  36-COD-RETORNO        TO  ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(654)      TO  ROTSAI-MSG-RETORNO
               PERFORM 030-000-FINALIZAR
           ELSE
               IF  36-COD-RETORNO EQUAL ZEROS  OR  4
                   MOVE '0653'            TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(653)  TO  ROTSAI-MSG-RETORNO
                   PERFORM 030-000-FINALIZAR
               END-IF
           END-IF.

       010-010-FIM.

      ******************************************************************
       020-000-TRATAR-NEGOCIO               SECTION.
      ******************************************************************

WPG        PERFORM 020-020-OBTER-SEQUENCIA.
WPG        PERFORM 020-030-GRAVAR-PESSOA.

           PERFORM VARYING IND-ENT FROM 1 BY 1
             UNTIL IND-ENT GREATER 70
                OR ROTENT-DANO-OPER-DESC(IND-ENT) EQUAL ZEROS
                   PERFORM 020-070-OBTER-MENOR-DATA
                   PERFORM 020-050-GRAVA-OCORRENCIAS
                   PERFORM 020-080-OBTER-DATA-VIGENTE
                   PERFORM 020-060-GRAVAR-LOG-OPERACOES
           END-PERFORM.

       020-000-FIM.

      ******************************************************************
       020-070-OBTER-MENOR-DATA          SECTION.
      ******************************************************************
      *    OBTEM MENOR DATA PARA GRAVACAO DO LOG

4S2511*    MOVE  ROTENT-CCNPJ-CPF               TO 36-CCNPJ-CPF.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ             TO 36-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CCNPJ-CPF               TO 36-CCNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ             TO 36-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF          TO 36-CCTRL-CNPJ-CPF
           MOVE  1                              TO 36-CTPO-ISENC-DESC.
           MOVE  ROTENT-DANO-OPER-DESC(IND-ENT) TO 36-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC(IND-ENT) TO 36-NSEQ-OPER-DESC.
           MOVE  'PARCIAL-05'                   TO 36-INSTRUCAO.
           MOVE  'DCOM6680'                     TO WRK-MODULO.

           CALL  WRK-MODULO           USING    36-ARGUMENTOS-ENTRADA
                                               36-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  36-COD-RETORNO             EQUAL 99
               MOVE  '0099'               TO  ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(099)
                                          TO  ROTSAI-MSG-RETORNO
               PERFORM 030-000-FINALIZAR
           END-IF.

           IF  36-COD-RETORNO EQUAL 3
               MOVE  'N'                  TO WRK-TRATA-ALTERACAO
               MOVE  ZEROS                TO  X1-WREG-LOG-ANTES-LEN
               MOVE  SPACES               TO  X1-WREG-LOG-ANTES-TEXT
           ELSE
               IF 36-COD-RETORNO          EQUAL ZEROS
                  MOVE 'S'                TO WRK-TRATA-ALTERACAO
                  MOVE  LENGTH OF 36-OCORRENCIAS(1)
                                             TO  X1-WREG-LOG-ANTES-LEN
                  MOVE  36-OCORRENCIAS(1)    TO  X1-WREG-LOG-ANTES-TEXT
               END-IF
           END-IF.

       020-070-FIM.

      ******************************************************************
       020-080-OBTER-DATA-VIGENTE        SECTION.
      ******************************************************************
      *    OBTEM MENOR DATA PARA GRAVACAO DO LOG

4S2511*    MOVE  ROTENT-CCNPJ-CPF               TO 36-CCNPJ-CPF.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ             TO 36-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CCNPJ-CPF               TO 36-CCNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ             TO 36-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF          TO 36-CCTRL-CNPJ-CPF
           MOVE  1                              TO 36-CTPO-ISENC-DESC.
           MOVE  ROTENT-DANO-OPER-DESC(IND-ENT) TO 36-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC(IND-ENT) TO 36-NSEQ-OPER-DESC.
           MOVE  'PARCIAL-06'                   TO 36-INSTRUCAO.
           MOVE  'DCOM6680'                     TO WRK-MODULO.

           CALL  WRK-MODULO           USING    36-ARGUMENTOS-ENTRADA
                                               36-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  36-COD-RETORNO             EQUAL 99
               MOVE  '0099'               TO  ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(099)
                                          TO  ROTSAI-MSG-RETORNO
               PERFORM 030-000-FINALIZAR
           END-IF.

           IF  36-COD-RETORNO EQUAL 3
               MOVE  ZEROS                   TO  X1-WREG-LOG-APOS-LEN
               MOVE  SPACES                  TO  X1-WREG-LOG-APOS-TEXT
           ELSE
               IF 36-COD-RETORNO        EQUAL ZEROS
                  MOVE  LENGTH OF 36-OCORRENCIAS(1)
                                             TO  X1-WREG-LOG-APOS-LEN
                  MOVE  36-OCORRENCIAS(1)    TO  X1-WREG-LOG-APOS-TEXT
               END-IF
           END-IF.

       020-070-FIM.

      ******************************************************************
       020-010-OBTER-TIMESTAMP            SECTION.
      ******************************************************************


      * OBTER TIMESTAMP

           MOVE  'DCOM7999'   TO  WRK-MODULO.

           CALL  WRK-MODULO   USING  WRK-TIMESTAMP-ATUAL
                                     LKG-ERRO-AREA
                                     LKG-SQLCA.


           CALL  WRK-MODULO   USING  WRK-TIMESTAMP-ATUAL-D
                                     LKG-ERRO-AREA
                                     LKG-SQLCA.

       020-010-FIM.

      ******************************************************************
       020-020-OBTER-SEQUENCIA        SECTION.
      ******************************************************************


           MOVE  ROTENT-COD-RETORNO    TO  WRK-COD-RETORNO-E.
           MOVE  ROTENT-MSG-RETORNO    TO  WRK-MSG-RETORNO-E.
           MOVE  ROTENT-RESTART        TO  WRK-RESTART-E.
           MOVE  ROTENT-FLAG           TO  WRK-FLAG-E.
           MOVE  ROTENT-CFUNC-BDSCO    TO  WRK-CFUNC-BDSCO-E.
           MOVE  ROTENT-CTERM          TO  WRK-CTERM-E.

      * PESQUISA SEQUENCIA
      *
           MOVE  13                    TO  WRK-CELMTO-DESC-COML-E.
           MOVE  1                     TO  WRK-WCHAVE-PRINC-ELMTO-E.
           MOVE  3                     TO  WRK-QDIG-SEQ-ELMTO-E.
           MOVE  'DCOM5022'            TO  WRK-MODULO.

           CALL  WRK-MODULO   USING  WRK-ENTRADA
                                     WRK-SAIDA
                                     LKG-ERRO-AREA
                                     LKG-SQLCA.

           IF  WRK-COD-RETORNO-S  NOT EQUAL  ZEROS
               MOVE  '0657'               TO  ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(657)
                                          TO  ROTSAI-MSG-RETORNO
               PERFORM 030-000-FINALIZAR
           END-IF.

       020-020-FIM.

     *******************************************************************
       020-030-GRAVAR-PESSOA              SECTION.
     *******************************************************************

      * INCLUIR REGISTRO TABELA PESSOA-ISENCAO

4S2511*    MOVE  ROTENT-CCNPJ-CPF          TO    48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF          TO    48-CCNPJ-CPF-ST.
           MOVE  WRK-NULT-SEQ-ELMTO-S      TO    48-NSEQ-PSSOA-DESC.
           MOVE  1                         TO    48-CTPO-ISENC-DESC.
WPG        MOVE  WRK-TIMESTAMP-ATUAL-D     TO    48-DINIC-VGCIA-ISENC
                                                 48-DFIM-VGCIA-ISENC.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ        TO    48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ        TO    48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF     TO    48-CCTRL-CNPJ-CPF.
           MOVE  ZEROS                     TO    48-CCLUB.
      *    MOVE  CADURT-CLUB (1)           TO    48-CCLUB.
      *    MOVE  3                         TO    48-CCLUB.
           MOVE  3                         TO    48-CABRGC-ISENC-DESC.

           IF    ROTENT-CFLIAL-CNPJ      EQUAL           '0000'
4S2511     OR    ROTENT-CFLIAL-CNPJ      EQUAL           SPACES
4S2511     OR    ROTENT-CFLIAL-CNPJ      EQUAL           LOW-VALUES
                 MOVE 'FI'                 TO      48-CINDCD-PSSOA-DESC
           ELSE
                 MOVE 'PJ'                 TO      48-CINDCD-PSSOA-DESC
           END-IF.

           MOVE  SPACES                    TO      48-CINDCD-REST-CDENT.
           MOVE  ROTENT-CFUNC-BDSCO        TO      48-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM              TO      48-CTERM.
           MOVE  'DCOM6742'                TO      WRK-MODULO.

           CALL      WRK-MODULO         USING
                                               48-ARGUMENTOS-ENTRADA
                                               48-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  48-COD-RETORNO NOT EQUAL ZEROS
               IF  48-COD-RETORNO EQUAL 4
                   MOVE '0546'            TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(546)
                                          TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'            TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(658)
                                          TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 030-000-FINALIZAR
           END-IF.

       020-030-FIM.

      ******************************************************************
       020-050-GRAVA-OCORRENCIAS           SECTION.
      ******************************************************************
      * INCLUIR TABELA OPERACAO-PESSOA-ISENCAO


4S2511*    MOVE  ROTENT-CCNPJ-CPF               TO 36-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF               TO 36-CCNPJ-CPF-ST.
           MOVE  WRK-NULT-SEQ-ELMTO-S           TO 36-NSEQ-PSSOA-DESC.
WPG        MOVE  WRK-TIMESTAMP-ATUAL-D          TO 36-DINIC-VGCIA-ISENC
                                                   36-DFIM-VGCIA-ISENC.
           MOVE  1                              TO 36-CTPO-ISENC-DESC.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ             TO 36-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ             TO 36-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF          TO 36-CCTRL-CNPJ-CPF.
           MOVE  ROTENT-DANO-OPER-DESC(IND-ENT) TO 36-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC(IND-ENT) TO 36-NSEQ-OPER-DESC.
           MOVE  ROTENT-CAG-BCRIA     (IND-ENT) TO 36-CAG-BCRIA.
           MOVE  ROTENT-CCTA-BCRIA-CLI(IND-ENT) TO 36-CCTA-BCRIA-CLI.

           MOVE  ZEROS                     TO      36-CCLUB.
      *    MOVE  CADURT-CLUB (1)           TO      36-CCLUB.

           IF    ROTENT-CFLIAL-CNPJ        EQUAL   ZEROS
                  MOVE 'FI'                TO 36-CINDCD-PSSOA-DESC
           ELSE
                  MOVE 'PJ'                TO 36-CINDCD-PSSOA-DESC
           END-IF.

           MOVE WRK-TIMESTAMP-ATUAL      TO 36-HULT-ATULZ
           MOVE ROTENT-CFUNC-BDSCO       TO 36-CFUNC-BDSCO.
           MOVE ROTENT-CTERM             TO 36-CTERM.
           MOVE  'DCOM6682'                 TO   WRK-MODULO.

           CALL      WRK-MODULO         USING  36-ARGUMENTOS-ENTRADA
                                               36-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  36-COD-RETORNO NOT EQUAL ZEROS
               IF  36-COD-RETORNO EQUAL 4
                   MOVE '0546'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(546)
                                        TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(658)
                                        TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 030-000-FINALIZAR
           END-IF.

      * ATUALIZA A TABELA DE WORKFLOW

           MOVE  ROTENT-DANO-OPER-DESC(IND-ENT) TO G9-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC(IND-ENT) TO G9-NSEQ-OPER-DESC.
           MOVE  37                             TO G9-CEVNTO-DESC-COML.
           MOVE  22                             TO G9-CSIT-INIC-DESC.
           MOVE  22                             TO G9-CSIT-FNAL-DESC.
           MOVE  WRK-TIMESTAMP-ATUAL            TO G9-HSIT-INIC-DESC
                                                   G9-HSIT-FNAL-DESC.

           MOVE  'PARCIAL-02'                   TO G9-INSTRUCAO.
           MOVE  'DCOM7348'                     TO WRK-MODULO.

           CALL      WRK-MODULO         USING   G9-ARGUMENTOS-ENTRADA
                                                G9-RETORNO
                                                LKG-ERRO-AREA
                                                LKG-SQLCA.

           IF  G9-COD-RETORNO NOT EQUAL ZEROS
               IF  G9-COD-RETORNO EQUAL 4
                   MOVE '0100'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(100)
                                        TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(658)
                                        TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 030-000-FINALIZAR
           END-IF.

LB         PERFORM 020-051-ACESSAR-DCOM7320.

           IF  G4RT-CELMTO-DESC-COML(1)         EQUAL 9
               PERFORM 32000-TRATAR-CONVENIO-CLIENTE
           END-IF.

           MOVE  ROTENT-DANO-OPER-DESC(IND-ENT) TO G9-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC(IND-ENT) TO G9-NSEQ-OPER-DESC.
           MOVE  35                             TO G9-CEVNTO-DESC-COML.
           MOVE  22                             TO G9-CSIT-INIC-DESC.

           IF (G4RT-CMEIO-ENTRD-DESC(1)         EQUAL 3 OR 4) OR
              (G4RT-CMEIO-ENTRD-DESC(1)         EQUAL 2 AND
               G4RT-CTPO-DESC-COML  (1)         EQUAL 5 AND
               12RT-CINDCD-CONVE-ESPCL(1)       EQUAL 'S')
               MOVE  21                         TO G9-CSIT-FNAL-DESC
           ELSE
               MOVE  22                         TO G9-CSIT-FNAL-DESC
           END-IF.

           MOVE  WRK-TIMESTAMP-ATUAL            TO G9-HSIT-INIC-DESC
                                                   G9-HSIT-FNAL-DESC.

           MOVE  'PARCIAL-02'                   TO G9-INSTRUCAO.
           MOVE  'DCOM7348'                     TO WRK-MODULO.

           CALL      WRK-MODULO         USING   G9-ARGUMENTOS-ENTRADA
                                                G9-RETORNO
                                                LKG-ERRO-AREA
                                                LKG-SQLCA.

           IF  G9-COD-RETORNO NOT EQUAL ZEROS
               IF  G9-COD-RETORNO EQUAL 4
                   MOVE '0100'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(100)
                                        TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(658)
                                        TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 030-000-FINALIZAR
           END-IF.

       020-050-FIM.

LB    ******************************************************************
LB     020-051-ACESSAR-DCOM7320            SECTION.
LB    ******************************************************************
LB    *ACESSAR DCOMB0G4 PARA OBTER O MEIO DE ENTRADA
LB
LB
LB         INITIALIZE G4-ARGUMENTOS-ENTRADA
LB                    G4-RETORNO
LB                    LKG-ERRO-AREA
LB                    LKG-SQLCA.
LB
LB         MOVE ROTENT-DANO-OPER-DESC(IND-ENT) TO G4-DANO-OPER-DESC.
LB         MOVE ROTENT-NSEQ-OPER-DESC(IND-ENT) TO G4-NSEQ-OPER-DESC.
LB         MOVE 'TOTAL'                        TO G4-INSTRUCAO.
LB         MOVE 'DCOM7320'                     TO WRK-MODULO.
LB
LB         CALL  WRK-MODULO         USING     G4-ARGUMENTOS-ENTRADA
LB                                            G4-RETORNO
LB                                            LKG-ERRO-AREA
LB                                            LKG-SQLCA.
LB
LB
LB         IF  G4-COD-RETORNO           EQUAL 99
LB             MOVE  G4-COD-RETORNO     TO  ROTSAI-COD-RETORNO
LB             MOVE  G4-MENSAGEM        TO  ROTSAI-MSG-RETORNO
LB             PERFORM 030-000-FINALIZAR
LB         END-IF.
LB
LB     020-051-FIM.

      *----------------------------------------------------------------*
       32000-TRATAR-CONVENIO-CLIENTE   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 12-ARGUMENTOS-ENTRADA
                      12-RETORNO.

           MOVE G4RT-CCONTR-CONVE-DESC(IND-ENT)  TO 12-CCONVE-CLI-DESC
           MOVE G4RT-CVRSAO-CONTR-CONVE(IND-ENT) TO 12-CVRSAO-CONVE-CLI
           MOVE 'TOTAL'                          TO 12-INSTRUCAO
           MOVE 'DCOM6560'                       TO WRK-MODULO.

           CALL WRK-MODULO             USING 12-ARGUMENTOS-ENTRADA
                                             12-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.

           IF  12-COD-RETORNO          NOT EQUAL ZEROS
               IF  12-COD-RETORNO          EQUAL  003
                   MOVE '1092'             TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(1092)  TO ROTSAI-MSG-RETORNO
                   PERFORM 030-000-FINALIZAR
               ELSE
                   MOVE 12-COD-RETORNO     TO ROTSAI-COD-RETORNO
                   MOVE 12-MENSAGEM        TO ROTSAI-MSG-RETORNO
                   PERFORM 030-000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       32000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
       020-060-GRAVAR-LOG-OPERACOES        SECTION.
      ******************************************************************
      *GRAVAR  LOG TABELA OPERACAO-PESSOA-ISENCAO

           MOVE  2                         TO  X1-CELMTO-DESC-COML.
           MOVE  57                        TO  X1-CEVNTO-LOG-DESC.
           MOVE  'I'                       TO  X1-CINDCD-MANUT-REG.

           INITIALIZE X1-CCHAVE-ELMTO-DESC.

           MOVE ROTENT-CCNPJ-CPF         TO X1-CCHAVE-ELMTO-DESC(01:09).
           MOVE ROTENT-CFLIAL-CNPJ       TO X1-CCHAVE-ELMTO-DESC(10:04).
           MOVE ROTENT-CCTRL-CNPJ-CPF    TO X1-CCHAVE-ELMTO-DESC(14:02).

           MOVE  ZEROS                   TO  X1-WREG-LOG-ANTES-LEN.
           MOVE  SPACES                  TO  X1-WREG-LOG-ANTES-TEXT.
           MOVE  LENGTH OF 36-COLUNAS-DA-TABELA
                                         TO  X1-WREG-LOG-APOS-LEN.
           MOVE  36-COLUNAS-DA-TABELA    TO  X1-WREG-LOG-APOS-TEXT.
           MOVE  'DCOM5054'              TO  X1-CTRANS-PROG-ATULZ.
           MOVE  'O'                     TO  X1-CINDCD-MODLD-PROCM.
           MOVE  237                     TO  X1-CEMPR-INC.
           MOVE  4130                    TO  X1-CDEPDC.
           MOVE  ROTENT-CFUNC-BDSCO      TO  X1-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM            TO  X1-CTERM.
           MOVE  'DCOM5075'              TO  WRK-MODULO.

           CALL      WRK-MODULO         USING
                                               X1-REGISTRO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF        X1-COD-RETORNO NOT EQUAL ZEROS
                     MOVE  '0099'          TO  ROTSAI-COD-RETORNO
                     MOVE  SG-MENSAGEM(578)
                                           TO  ROTSAI-MSG-RETORNO
                     PERFORM 030-000-FINALIZAR
           END-IF.

       020-060-FIM.

      ******************************************************************
       030-000-FINALIZAR                  SECTION.
      ******************************************************************

           GOBACK.

       030-000-FIM.
