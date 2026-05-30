      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID.     DCOM0607.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0607                                    *
      *    PROGRAMADOR.:   FABRICA - ALTRAN                            *
      *    ANALISTA....:   EMERSON ARTUR FRANCO                        *
      *    DATA........:   21/05/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA OPERACOES - POR AGENCIA            *
      *                                                                *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMFGTM - CONSULTA OPERACOES - POR AGENCIA                 *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSY - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0602           CHAMA: DCOM0612            *
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
       01  FILLER                          PIC  X(050)     VALUE
           '*** DCOM0607 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                         PIC S9(08) COMP VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                       PIC S9(08) COMP VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                       PIC S9(08) COMP VALUE +49369.
      *
       01  WRK-UNNS                        PIC S9(08) COMP VALUE +209.
       01  WRK-UNNS-C                      PIC S9(08) COMP VALUE +49361.
       01  WRK-PNNS                        PIC S9(08) COMP VALUE +225.
       01  WRK-PNNS-C                      PIC S9(08) COMP VALUE +49377.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  IND-1                       PIC  9(005)     VALUE ZEROS.
           05  WRK-MODULO                  PIC  X(008)     VALUE SPACES.
           05  WRK-INCONSIS                PIC  X(001)     VALUE SPACES.
           05  WRK-MENS01                  PIC  X(072)     VALUE
               'DIGITE A OPERACAO DESEJADA'.
           05  WRK-MENS02                  PIC  X(072)     VALUE
               'PF INVALIDA'.
           05  WRK-MENS03                  PIC  X(072)     VALUE
               'CODIGO DA OPERACAO INVALIDA'.
           05  WRK-MENS04                  PIC  X(072)     VALUE
               'CAMPOS BRILHANTES INCONSISTENTES'.
           05  WRK-MENS05                  PIC  X(072)     VALUE
               'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM0602 / DCOM6204 ***'.
      *----------------------------------------------------------------*
      *
       01          COMU-AREA-IMS.
         05        COMU-LL                 PIC S9(04) COMP VALUE +406.
         05        COMU-ZZ                 PIC S9(04) COMP VALUE ZEROS.
         05        COMU-TRANCODE.
            10     COMU-TRANSACAO          PIC X(008)      VALUE SPACES.
            10     FILLER                  PIC X(007)      VALUE SPACES.
         05        COMU-SENHAS             PIC X(037)      VALUE SPACES.
         05        COMU-COMANDO            PIC X(068)      VALUE SPACES.
         05        COMU-TIMESTAMP          PIC X(026)      VALUE SPACES.
         05        COMU-MENSAGEM           PIC X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-SQLCA                   PIC  X(136)     VALUE SPACES.
           05  WRK-GU                      PIC  X(004)     VALUE 'GU  '.
           05  WRK-CHNG                    PIC  X(004)     VALUE 'CHNG'.
           05  WRK-ISRT                    PIC  X(004)     VALUE 'ISRT'.
           05  WRK-FUNCAO                  PIC  X(004)     VALUE SPACES.
           05  WRK-TELA                    PIC  X(008)     VALUE SPACES.
           05  WRK-VERSAO                  PIC  X(006)     VALUE
                                                           'VRS002'.
           05  WRK-COD-USER                PIC  X(007)     VALUE SPACES.
           05  WRK-COD-USER-R              REDEFINES
               WRK-COD-USER                PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER                  PIC  X(002)     VALUE SPACES.
               10  WRK-COD-DEPTO-N         PIC  9(004)     VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER                  PIC  X(004)     VALUE SPACES.
               10  WRK-TRANSACAO           PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(1988)    VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM7995 **'.
      *----------------------------------------------------------------*

       01  WRK-ENTRADA-DCOM7995.
           05  WRK-ENT-OPCAO           PIC  X(02)          VALUE SPACES.
           05  WRK-ENT-COD-FUNC        PIC  9(09)          VALUE ZEROS.
           05  WRK-ENT-COD-TERMINAL    PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-ENT-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-ENT-DADOS           PIC  X(1600)        VALUE SPACES.

       01  WRK-SAIDA-DCOM7995.
           05  WRK-SAI-COD-RETORNO     PIC  9(02)          VALUE ZEROS.
           05  WRK-SAI-MSG-RETORNO     PIC  X(79)          VALUE SPACES.
           05  WRK-SAI-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-SAI-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-SAI-DADOS           PIC  X(1600)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFGTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFGTM.
           05  WRK-660-LL-AREA-FGTM        PIC  9(04) COMP VALUE 0022.
           05  WRK-660-LL-MENSAGEM-FGTM    PIC  9(04) COMP VALUE 0230.
           05  WRK-660-SENHAS-FGTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FGTM        PIC  9(04) COMP VALUE 0068.
           05  WRK-660-TIMESTAMP-FGTM      PIC  9(04) COMP VALUE 0026.
           05  WRK-660-TIPOOPC-FGTM        PIC  9(04) COMP VALUE 0001.
           05  WRK-660-TIPOCONS-FGTM       PIC  9(04) COMP VALUE 0001.
           05  WRK-660-PASSO-FGTM          PIC  9(04) COMP VALUE 0001.
           05  WRK-660-DANO-FGTM           PIC  9(04) COMP VALUE 2006.
           05  WRK-660-NSEQ-FGTM           PIC  9(04) COMP VALUE 2011.
           05  WRK-660-MENSA-FGTM          PIC  9(04) COMP VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05  FILLER                      PIC  X(045)     VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                      PIC  X(012)     VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE             PIC  9(002)     VALUE ZEROS.
           05  FILLER                      PIC  X(011)     VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO              PIC  9(002)     VALUE ZEROS.
           05  FILLER                      PIC  X(002)     VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFGTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FGTM.
           05  FILLER                      PIC  X(018)     VALUE SPACES.
           05  WRK-INP-PFK-FGTM            PIC  X(001)     VALUE SPACES.
           05  WRK-INP-SENHAS-FGTM         PIC  X(037)     VALUE SPACES.
           05  WRK-INP-COMANDO-FGTM        PIC  X(068)     VALUE SPACES.
           05  WRK-INP-TIMESTAMP-FGTM      PIC  X(026)     VALUE SPACES.
           05  WRK-INP-TIPOOPC-FGTM        PIC  X(001)     VALUE SPACES.
           05  WRK-INP-TIPOCONS-FGTM       PIC  X(001)     VALUE SPACES.
           05  WRK-INP-PASSO-FGTM          PIC  X(001)     VALUE SPACES.
           05  WRK-INP-DADOS-FGTM.
               10  WRK-INP-DANO-FGTM-X.
                   15  WRK-INP-DANO-FGTM   PIC  9(004)     VALUE ZEROS.
               10  WRK-INP-NSEQ-FGTM-X.
                   15  WRK-INP-NSEQ-FGTM   PIC  9(009)     VALUE ZEROS.
           05  WRK-INP-MENSA-FGTM          PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFGTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-FGTM.
           05  WRK-OUT-LL-FGTM             PIC S9(04) COMP VALUE +0230.
           05  WRK-OUT-ZZ-FGTM             PIC  9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FGTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-FGTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-TIMESTAMP-FGTM      PIC  X(26)      VALUE SPACES.
           05  WRK-OUT-TIPOOPC-FGTM        PIC  X(01)      VALUE SPACES.
           05  WRK-OUT-TIPOCONS-FGTM       PIC  X(01)      VALUE SPACES.
           05  WRK-OUT-PASSO-FGTM          PIC  X(01)      VALUE SPACES.
           05  WRK-OUT-DADOS-FGTM.
               10  WRK-OUT-DANO-ATTR-FGTM  PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-DANO-FGTM-X.
                   15  WRK-OUT-DANO-FGTM   PIC  9(004)     VALUE ZEROS.
               10  WRK-OUT-NSEQ-ATTR-FGTM  PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-NSEQ-FGTM-X.
                   15  WRK-OUT-NSEQ-FGTM   PIC  9(009)     VALUE ZEROS.
      *
           05  WRK-OUT-MENSA-FGTM          PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-PFK.
           05  COMU-LL-PFK                 PIC S9(04) COMP VALUE +124.
           05  COMU-ZZ-PFK                 PIC S9(04) COMP VALUE ZEROS.
           05  COMU-TRANCODE-PFK.
               10  COMU-TRANSACAO-PFK      PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  COMU-PFK                PIC  X(001)     VALUE SPACES.
           05  COMU-SENHAS-PFK             PIC  X(037)     VALUE SPACES.
           05  COMU-COMANDO-PFK            PIC  X(068)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(079)     VALUE
           '*** AREA PARA ACESSO AO DCOM5537 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ4'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA PARA VALIDACAO ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXE'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** COMU AREA DO IMS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ2'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** DCOM0607 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                             SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM                 PIC  X(008).
           05 FILLER                       PIC  X(002).
           05 LNK-IO-STATUS                PIC  X(002).
           05 FILLER                       PIC  X(012).
           05 LNK-IO-MODNAME               PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM                PIC  X(008).
           05 FILLER                       PIC  X(002).
           05 LNK-ALT-STATUS               PIC  X(002).
           05 FILLER                       PIC  X(012).
           05 LNK-ALT-MODNAME              PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE WRK-GU                     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF  RETURN-CODE                 EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                     TO WRK-OUT-DADOS-FGTM
                                              WRK-OUT-SENHAS-FGTM
                                              WRK-OUT-COMANDO-FGTM.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     PROCESSAR MENSAGEM                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMFGTM'                 TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0607'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFGTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFGTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE               SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-MENSAGEM               TO COMU-AREA-IMS

           MOVE COMU-TIMESTAMP             TO WRK-OUT-TIMESTAMP-FGTM
                                              WRK-INP-TIMESTAMP-FGTM
                                              WRK-ENT-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-OPCAO                  TO WRK-OUT-TIPOOPC-FGTM

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0607'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FGTM     TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           IF  COMU-MENSAGEM               EQUAL SPACES
               MOVE WRK-MENS01             TO WRK-OUT-MENSA-FGTM
           ELSE
               MOVE COMU-MENSAGEM          TO WRK-OUT-MENSA-FGTM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8200-ACESSAR-DCOM7995           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE 'DCOM0601'             TO WRK-ENT-TRANS

           MOVE 'DCOM7995'             TO WRK-MODULO.

           CALL WRK-MODULO          USING WRK-ENTRADA-DCOM7995
                                          WRK-SAIDA-DCOM7995
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       8200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFGTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFGTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FGTM.

           PERFORM 2210-DEVOLVER-TELA

           EVALUATE TRUE

               WHEN WRK-INP-PFK-FGTM       EQUAL 'H' OR
                   (WRK-INP-PFK-FGTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FGTM   EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-FGTM  TO WRK-OUT-MENSA-FGTM

               WHEN WRK-INP-PFK-FGTM       EQUAL '3' OR
                   (WRK-INP-PFK-FGTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FGTM   EQUAL 'PFK03')
                    PERFORM 2240-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FGTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FGTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FGTM   EQUAL 'PFK05')
                    PERFORM 2250-MENU-ROTINAS

               WHEN WRK-INP-PFK-FGTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FGTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FGTM   EQUAL 'PFK10')
                    PERFORM 2260-MENU-DCOM

               WHEN (WRK-INP-PFK-FGTM      EQUAL '.' AND
                     WRK-INP-COMANDO-FGTM  EQUAL SPACES)
                     PERFORM 2270-TRATAR-ENTER

               WHEN OTHER
                    MOVE WRK-MENS02          TO  WRK-OUT-MENSA-FGTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA                                              *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-DANO-FGTM-X     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-DANO-FGTM
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-NSEQ-FGTM-X     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-NSEQ-FGTM
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           MOVE  WRK-INP-TIMESTAMP-FGTM    TO WRK-OUT-TIMESTAMP-FGTM
           MOVE  WRK-INP-TIPOOPC-FGTM      TO WRK-OUT-TIPOOPC-FGTM
           MOVE  WRK-INP-DANO-FGTM-X       TO WRK-OUT-DANO-FGTM-X.
           MOVE  WRK-INP-NSEQ-FGTM-X       TO WRK-OUT-NSEQ-FGTM-X.

      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR                              *
      *----------------------------------------------------------------*
       2240-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTAMP-FGTM     TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
           END-PERFORM.


           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FGTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-IMS
           MOVE WRK-INP-TIMESTAMP-FGTM     TO COMU-TIMESTAMP
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0607'                 TO COMU-TRANSACAO
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2250-MENU-ROTINAS                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTAMP-FGTM     TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0607'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE '5'                        TO COMU-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2250-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU DCOM                                      *
      *----------------------------------------------------------------*
       2260-MENU-DCOM                      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTAMP-FGTM     TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0607'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE 'A'                        TO COMU-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2260-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER                   SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO  WRK-INCONSIS

           PERFORM 2271-CONSISTIR-FISICA

           IF  WRK-INCONSIS                EQUAL  '*'
               GO TO 2270-99-FIM
           END-IF.

           PERFORM 2290-TRANSFERIR-PGM.

      *----------------------------------------------------------------*
       2270-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA               SECTION.
      *----------------------------------------------------------------*

           IF (WRK-INP-DANO-FGTM-X         EQUAL SPACES OR LOW-VALUES)OR
              (WRK-INP-NSEQ-FGTM-X         EQUAL SPACES OR LOW-VALUES)
               MOVE WRK-49369              TO WRK-OUT-DANO-ATTR-FGTM
                                              WRK-OUT-NSEQ-ATTR-FGTM
               MOVE 'OPERACAO OBRIGATORIA' TO WRK-OUT-MENSA-FGTM
               MOVE '*'                    TO WRK-INCONSIS
               GO TO 2271-99-FIM
           END-IF.

           IF (WRK-INP-DANO-FGTM-X         EQUAL SPACES OR LOW-VALUES OR
               WRK-INP-DANO-FGTM           NOT NUMERIC) OR
              (WRK-INP-NSEQ-FGTM-X         EQUAL SPACES OR LOW-VALUES OR
               WRK-INP-NSEQ-FGTM           NOT NUMERIC)
               MOVE WRK-49369              TO WRK-OUT-DANO-ATTR-FGTM
                                              WRK-OUT-NSEQ-ATTR-FGTM
               MOVE WRK-MENS03             TO WRK-OUT-MENSA-FGTM
               MOVE '*'                    TO WRK-INCONSIS
           END-IF.

           PERFORM 2295-BUSCA-DADOS.

           IF  WRK-INCONSIS                EQUAL SPACES
               PERFORM 1253-10-VALIDAR-PERFIL
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1253-10-VALIDAR-PERFIL          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           MOVE 'DCOM6437'             TO WRK-MODULO.
           MOVE WRK-COD-USER-R         TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE 5537-SAI-CAG-BCRIA     TO ROTENT-6437-AGEPESQ-ENV.
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.

           CALL WRK-MODULO          USING ROTENT-6437-ENVIO
                                          ROTSAI-6437-RETORNO
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET    EQUAL 99
               MOVE  'DB2'            TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  ROTSAI-6437-COD-RET        NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET      EQUAL 05 OR
               ROTSAI-6437-RESUL-RET      EQUAL 04
               NEXT SENTENCE
           ELSE
               GO TO 1253-10-99-FIM
           END-IF.

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           MOVE 'DCOM6437'             TO WRK-MODULO.
           MOVE WRK-COD-USER-R         TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE 5537-SAI-CAG-OPER-OPER TO ROTENT-6437-AGEPESQ-ENV.
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.

           CALL WRK-MODULO          USING ROTENT-6437-ENVIO
                                          ROTSAI-6437-RETORNO
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET    EQUAL 99
               MOVE  'DB2'            TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  ROTSAI-6437-COD-RET        NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET      EQUAL 05 OR
               ROTSAI-6437-RESUL-RET      EQUAL 04
               MOVE '*'                   TO WRK-INCONSIS
               MOVE 'USUARIO NAO AUTORIZADO'
                                          TO WRK-OUT-MENSA-FGTM
           END-IF.

      *----------------------------------------------------------------*
       1253-10-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2295-BUSCA-DADOS                    SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-DANO-FGTM   TO  5537-ENT-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-FGTM   TO  5537-ENT-NSEQ-OPER-DESC
           MOVE ZEROS               TO  5537-ENT-COD-RETORNO
           MOVE SPACES              TO  5537-ENT-MSG-RETORNO
           MOVE ZEROS               TO  5537-ENT-RESTART

           IF  WRK-INP-TIPOOPC-FGTM EQUAL 1
               MOVE 'O'             TO  5537-ENT-FLAG
           ELSE
               MOVE 'H'             TO  5537-ENT-FLAG
               MOVE SPACES          TO  5537-ENT-HULT-ATULZ
           END-IF.

           MOVE WRK-COD-USER-R      TO  5537-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM        TO  5537-ENT-CTERM

           MOVE 'DCOM5537'          TO  WRK-MODULO

           CALL WRK-MODULO    USING           5537-ENTRADA-ROTEADOR
                                              5537-SAIDA-ROTEADOR
                                              ERRO-AREA
                                              WRK-SQLCA.

           EVALUATE 5537-SAI-COD-RETORNO
                   WHEN '0000'
                         CONTINUE
                   WHEN '0099'
                         MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                         PERFORM 9999-PROCESSAR-ROTINA-ERRO

                   WHEN OTHER
                         MOVE 5537-SAI-MSG-RETORNO
                                         TO WRK-OUT-MENSA-FGTM
                         MOVE '*'        TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       2295-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *
      *----------------------------------------------------------------*
       2290-TRANSFERIR-PGM                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FGTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0607'             TO WRK-CHAMADOR(IND-1)
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-INP-DANO-FGTM          TO WRK-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-FGTM          TO WRK-NSEQ-OPER-DESC.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995

           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL
           MOVE ZEROS                      TO COMU-ZZ

           MOVE  'DCOM0607'                TO COMU-TRANSACAO

           IF  WRK-OPCAO                   EQUAL 2
               MOVE  'DCOM0617'            TO WRK-TELA
           ELSE
               MOVE  'DCOM0612'            TO WRK-TELA
           END-IF.

           MOVE  COMU-AREA-IMS             TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2290-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FGTM
                                       TO  WRK-660-LL-MENSAGEM-FGTM
                                           WRK-OUT-LL-FGTM.
           MOVE LENGTH OF WRK-660-DCOMFGTM
                                       TO  WRK-660-LL-AREA-FGTM.

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FGTM
                                              WRK-660-DCOMFGTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO
           MOVE WRK-OUTPUT-FGTM            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                      SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0607'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0607'             TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER               TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO              TO ERR-COD-DEPTO.

           IF  ERR-TIPO-ACESSO             EQUAL 'DB2'
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
