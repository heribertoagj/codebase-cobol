      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0402.
       AUTHOR.         ZEZITO.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0402                                     *
      *     PROGRAMADOR.: ZEZITO                                       *
      *     ANALISTA....: DANILO                                       *
      *     DATA........: 01/02/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: MENU INICIAL > OPCAO 04 DO SISTEMA DCOM:     *
      *                                > LIBERACAO OPERACAO            *
      *                   DIRECIONAR PARA A TELA CONFORME OPCAO        *
      *                   SELECIONADA.                                 *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMDBTM....: LIBERACAO DA OPERACAO - MENU                 *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *     I#DCOMXF - AREA DE COMUNICAO DCOM6438.                     *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     DCOM6438 - VERIFICAR DISPONIBILIDADE DE SISTEMAS.          *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0401           CHAMA: DCOM0402           *
      *                                             DCOM0403           *
      *                                             DCOM0404           *
      *                                             DCOM0405           *
      *                                             DCOM0406           *
      *                                             DCOM0407           *
      *                                             DCOM0408           *
      *                                             DCOM0409           *
      *                                             DCOM0410           *
      *                                             DCOM0411           *
      *================================================================*
BRQ001*                   BRQ DIGITAL SOLUTIONS - ALTERACAO            *
BRQ001*----------------------------------------------------------------*
BRQ001*     ANALISTA....: LUCIANDRA SILVEIRA                           *
BRQ001*     ANALISTA DS.: RICARDO JAMMAL                               *
BRQ001*     DATA........: 23/01/2019                                   *
BRQ001*----------------------------------------------------------------*
BRQ001*     OBJETIVO....: INCLUIR OPCAO 4 - CONSULTAR PENDENTE         *
BRQ001*                   PROJETO 18/0456 - BORDERO ELETRONICO         *
BRQ001*================================================================*

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
      *           *** DCOM0402 - INICIO DA AREA DE WORKING ***         *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                ***     AREA DE ATIDUTOS ***                    *
      *----------------------------------------------------------------*

      *---  NUMERICO - NORMAL    - DESPROTEGIDO  -     POS. CURSOR  ---*
       01  WRK-49361                   PIC S9(008) COMP  VALUE +49361.

      *---  NUMERICO - BRILHANTE - DESPROTEGIDO  -     POS. CURSOR  ---*
       01  WRK-49369                   PIC S9(008) COMP  VALUE +49369.

      *----------------------------------------------------------------*
      *               *** VARIAVEIS AUXILIARES ***                     *
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.
           05  WRK-QTDE-SELECAO        PIC  9(003) COMP-3  VALUE ZEROS.
           05  WRK-AGENCIA-NUM         PIC  9(005)         VALUE ZEROS.
           05  WRK-AGENCIA-TELA        PIC  9(005)         VALUE ZEROS.
FS2511 01  WRK-CPFCGC.
FS2511*    05  WRK-NUMERO              PIC  9(009)         VALUE ZEROS.
FS2511     05  WRK-NUMERO              PIC  X(009)         VALUE SPACES.
FS2511*    05  WRK-FILIAL              PIC  9(005)         VALUE ZEROS.
FS2511     05  WRK-FILIAL              PIC  X(004)         VALUE SPACES.
FS2511 01  WRK-CONTROLE                PIC  9(002)         VALUE ZEROS.

       01  WRK-AREA-BRAD0431.
           05  WRK-CONTA-0431          PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TAMANHO-0431        PIC  9(002)         VALUE ZEROS.
           05  WRK-DIGITO-0431         PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
      *             *** AREAS AUXILIARES PARA POOL5005 ***             *
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)       VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)       VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)       VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)       VALUE SPACES.
           05  WRK-TELA                PIC  X(008)       VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)       VALUE 'VRS002'.
           05  WRK-COD-USER            PIC  X(007)       VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).

           05  WRK-COD-DEPTO.
               10  WRK-FILLER          PIC  X(002)       VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)       VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)       VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(1988)      VALUE SPACES.

      *----------------------------------------------------------------*
      *              *** AREA DE ERRO DA BRAD0660 ***                  *
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)       VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)       VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(011)       VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(002)       VALUE ' *'.

      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOM0402  -  INPUT ***                 *
      *----------------------------------------------------------------*

       01  INPUT-DBTM.
           05  FILLER                  PIC  X(018)         VALUE SPACES.
           05  INP-PFK                 PIC  X(001)         VALUE SPACES.
           05  INP-SENHAS              PIC  X(037)         VALUE SPACES.
           05  INP-COMANDO             PIC  X(068)         VALUE SPACES.
           05  INP-DADOS.
               10  INP-OCULTO.
                   15  INP-TPOOPC      PIC  X(001)         VALUE SPACES.
                   15  INP-TMSTAMP     PIC  X(026)         VALUE SPACES.
                   15  FILLER          PIC  X(041)         VALUE SPACES.
               10  INP-OPCAO.
                   15  INP-OPCAO-N     PIC  9(001)         VALUE ZEROS.
               10  INP-OPER.
                   15  INP-OPER-N      PIC  9(004)         VALUE ZEROS.
               10  INP-NSEQ.
                   15  INP-NSEQ-N      PIC  9(009)         VALUE ZEROS.
               10  INP-AGENCIA.
                   15  INP-AGENCIA-N   PIC  9(005)         VALUE ZEROS.
               10  INP-AGENC.
                   15  INP-AGENC-N     PIC  9(005)         VALUE ZEROS.
               10  INP-CONTA.
                   15  INP-CONTA-N     PIC  9(013)         VALUE ZEROS.
FS2511         10  INP-CNPJ.
FS2511*            15  INP-CNPJ-N      PIC  9(009)         VALUE ZEROS.
FS2511             15  INP-CNPJ-N      PIC  X(009)         VALUE SPACES.
FS2511         10  INP-FILIAL.
FS2511*            15  INP-FILIAL-N    PIC  9(004)         VALUE ZEROS.
FS2511             15  INP-FILIAL-N    PIC  X(004)         VALUE SPACES.
               10  INP-DIG.
                   15  INP-DIG-N       PIC  9(002)         VALUE ZEROS.
           05  INP-MENSAG              PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOM0402  -  OUTPUT ***                *
      *----------------------------------------------------------------*

       01  OUTPUT-DBTM.
           05  OUT-LL                  PIC S9(004) COMP    VALUE +0326.
           05  OUT-ZZ                  PIC S9(004) COMP    VALUE +0.
           05  OUT-SENHAS              PIC  X(037)         VALUE SPACES.
           05  OUT-COMANDO             PIC  X(068)         VALUE SPACES.
           05  OUT-DADOS.
               10  OUT-OCULTO.
                   15  OUT-TPOOPC      PIC  X(001)         VALUE SPACES.
                   15  OUT-TMSTAMP     PIC  X(026)         VALUE SPACES.
                   15  FILLER          PIC  X(041)         VALUE SPACES.
               10  OUT-OPCAO-ATTR      PIC S9(004) COMP    VALUE  +0.
               10  OUT-OPCAO.
                   15  OUT-OPCAO-N     PIC  9(001)         VALUE ZEROS.
               10  OUT-OPER-ATTR       PIC S9(004) COMP    VALUE  +0.
               10  OUT-OPER.
                   15  OUT-OPER-N      PIC  9(004)         VALUE ZEROS.
               10  OUT-NSEQ-ATTR       PIC S9(004) COMP    VALUE  +0.
               10  OUT-NSEQ.
                   15  OUT-NSEQ-N      PIC  9(009)         VALUE ZEROS.
               10  OUT-AGENCIA-ATTR    PIC S9(004) COMP    VALUE  +0.
               10  OUT-AGENCIA.
                   15  OUT-AGENCIA-N   PIC  9(005)         VALUE ZEROS.
               10  OUT-AGENC-ATTR      PIC S9(004) COMP    VALUE  +0.
               10  OUT-AGENC.
                   15  OUT-AGENC-N     PIC  9(005)         VALUE ZEROS.
               10  OUT-CONTA-ATTR      PIC S9(004) COMP    VALUE  +0.
               10  OUT-CONTA.
                   15  OUT-CONTA-N     PIC  9(013)         VALUE ZEROS.
               10  OUT-CNPJ-ATTR       PIC S9(004) COMP    VALUE  +0.
FS2511         10  OUT-CNPJ.
FS2511*            15  OUT-CNPJ-N      PIC  9(009)         VALUE ZEROS.
FS2511             15  OUT-CNPJ-N      PIC  X(009)         VALUE SPACES.
FS2511         10  OUT-FILIAL-ATTR     PIC S9(004) COMP    VALUE  +0.
FS2511         10  OUT-FILIAL.
FS2511*            15  OUT-FILIAL-N    PIC  9(004)         VALUE ZEROS.
FS2511             15  OUT-FILIAL-N    PIC  X(004)         VALUE SPACES.
               10  OUT-DIG-ATTR        PIC S9(004) COMP    VALUE  +0.
               10  OUT-DIG.
                   15  OUT-DIG-N       PIC  9(002)         VALUE ZEROS.
           05  OUT-MENSAG              PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOMDBTM - BRAD0660 ***                *
      *----------------------------------------------------------------*

       01  660-DCOMDBTM.
           05  660-LL-AREA-DBTM        PIC  9(004) COMP    VALUE 0030.
           05  660-LL-MENSAGEM-DBTM    PIC  9(004) COMP    VALUE 0326.
           05  660-SENHAS-DBTM         PIC  9(004) COMP    VALUE 0037.
           05  660-COMANDO-DBTM        PIC  9(004) COMP    VALUE 0068.
           05  660-OCULTO-DBTM         PIC  9(004) COMP    VALUE 0068.
           05  660-OPCAO-DBTM          PIC  9(004) COMP    VALUE 2003.
           05  660-OPER-DBTM           PIC  9(004) COMP    VALUE 2006.
           05  660-NSEQ-DBTM           PIC  9(004) COMP    VALUE 2011.
           05  660-AGENCIA-DBTM        PIC  9(004) COMP    VALUE 2007.
           05  660-AGENC-DBTM          PIC  9(004) COMP    VALUE 2007.
           05  660-CONTA-DBTM          PIC  9(004) COMP    VALUE 2015.
           05  660-CNPJ-DBTM           PIC  9(004) COMP    VALUE 2011.
           05  660-FILIAL-DBTM         PIC  9(004) COMP    VALUE 2006.
           05  660-DIG-DBTM            PIC  9(004) COMP    VALUE 2004.
           05  660-MENSA-DBTM          PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
      *                *** AREA DE COMUNICACAO ***                     *
      *----------------------------------------------------------------*

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP    VALUE +151.
           05  COMU-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK            PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPCAO          PIC  9(001)         VALUE ZEROS.
               10  COMU-TIMESTAMP      PIC  X(026)         VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO PFK ***                         *
      *----------------------------------------------------------------*

       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(004) COMP    VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK1           PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(068)         VALUE SPACES.

      *---------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO CHAMA DCOM0403                 *
      *---------------------------------------------------------------*

       01  COMU-AREA-UN.
           05  COMU-LL-UN              PIC S9(004) COMP    VALUE +294.
           05  COMU-ZZ-UN              PIC S9(004) COMP    VALUE +0.
           05  COMU-TRANCODE-UN.
               10  COMU-TRANSACAO-UN   PIC  X(008)         VALUE SPACES.
               10  FILLER-UN           PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-UN         PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-UN          PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-UN         PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-UN.
               10  COMU-OPCAO-UN       PIC  9(001)         VALUE ZEROS.
               10  COMU-OPER-UN        PIC  9(013)         VALUE ZEROS.
               10  COMU-AGENCIA-UN     PIC  9(005)         VALUE ZEROS.
               10  COMU-AGENC-UN       PIC  9(005)         VALUE ZEROS.
               10  COMU-CONTA-UN       PIC  9(013)         VALUE ZEROS.
FS2511*        10  COMU-CNPJ-UN        PIC  9(009)         VALUE ZEROS.
FS2511         10  COMU-CNPJ-UN        PIC  X(009)         VALUE SPACES.
FS2511*        10  COMU-FILIAL-UN      PIC  9(004)         VALUE ZEROS.
FS2511         10  COMU-FILIAL-UN      PIC  X(004)         VALUE SPACES.
               10  COMU-DIG-UN         PIC  9(002)         VALUE ZEROS.
               10  COMU-DANO-OPER-UN   PIC  9(004)         VALUE ZEROS.
               10  COMU-NSEQ-OPER-UN   PIC  9(009)         VALUE ZEROS.
               10  COMU-TIMESTAMP-UN   PIC  X(026)         VALUE ZEROS.
               10  COMU-MENSAGEM-UN    PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO DCOM6416                       *
      *---------------------------------------------------------------*

       01  WRK-6416-ENTRADA-ROTEADOR.
           05 WRK-6416E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-6416E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-6416E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-6416E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-6416E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-6416E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-6416E-BANCO          PIC  9(003)         VALUE ZEROS.
           05 WRK-6416E-DEPDC          PIC  9(005)         VALUE ZEROS.

       01  WRK-6416-SAIDA-ROTEADOR.
           05 WRK-6416S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-6416S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-6416S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-6416S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-6416S-AG-DEPDC       PIC  9(005)         VALUE ZEROS.
           05 WRK-6416S-DG-DEPDC       PIC  X(001)         VALUE SPACES.
           05 WRK-6416S-NOME-DEPDC     PIC  X(040)         VALUE SPACES.
           05 WRK-6416S-END-DEPDC      PIC  X(030)         VALUE SPACES.
           05 WRK-6416S-DIRETORIA-REG  PIC  9(005)         VALUE ZEROS.
           05 WRK-6416S-DIRETORIA-NOME PIC  X(040)         VALUE SPACES.
           05 WRK-6416S-NOME-BANCO     PIC  X(040)         VALUE SPACES.
           05 WRK-6416S-POLO-SERVICO   PIC  9(005)         VALUE ZEROS.
           05 WRK-6416S-MUNIC-DEPDC    PIC  X(025)         VALUE SPACES.
           05 WRK-6416S-UF             PIC  X(002)         VALUE SPACES.
           05 WRK-6416S-CEP            PIC  9(005)         VALUE ZEROS.
           05 WRK-6416S-CEP-COMPL      PIC  9(003)         VALUE ZEROS.
           05 WRK-6416S-INDIC-DEPDC-AT PIC  X(001)         VALUE SPACES.
           05 WRK-6416S-NATUREZA-DEPDC PIC  X(001)         VALUE SPACES.
           05 WRK-6416S-COD-DISTR-CORR PIC  9(005)         VALUE ZEROS.
           05 WRK-6416S-DENCRR-DEPDC-BDSCO
                                       PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DO PROGRAMA DCOM6408 ***                       *
      *----------------------------------------------------------------*

       COPY 'I#DCOMX3'.

      *----------------------------------------------------------------*
      *        *** AREA DO PROGRAMA DCOM6437 ***                       *
      *----------------------------------------------------------------*

       COPY 'I#DCOMXE'.
      *----------------------------------------------------------------*
      *        *** AREA DO PROGRAMA DCOM6438 ***                       *
      *----------------------------------------------------------------*

       COPY 'I#DCOMXF'.

      *----------------------------------------------------------------*
      *        *** AREA DA BRAD7100 ***                                *
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
      *        *** DCOM0402 - FIM DA AREA DE WORKING ***               *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE  WRK-GU                TO  WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR.
      *----------------------------------------------------------------*

           CALL  'POOL5005'            USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL  04 OR 12
               GOBACK
           END-IF.


           MOVE SPACES                 TO  OUT-DADOS
                                           OUT-SENHAS
                                           OUT-COMANDO.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'DCOMDBTM'            TO  WRK-TELA.

           IF WRK-TRANSACAO     NOT EQUAL  'DCOM0402'
              PERFORM  1100-RECEBER-CONTROLE
           ELSE
              PERFORM  2000-PROCESSAR-DCOMDBTM
           END-IF.

           IF WRK-TELA              EQUAL  'DCOMDBTM'
              PERFORM  3000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA.

           IF  WRK-TRANSACAO      NOT EQUAL 'DCOM0401'

               MOVE WRK-MENSAGEM       TO  COMU-AREA-UN
               MOVE COMU-SENHAS-UN     TO OUT-SENHAS
                                          INP-SENHAS
               MOVE COMU-TIMESTAMP-UN  TO OUT-TMSTAMP

             EVALUATE WRK-TRANSACAO
               WHEN  'DCOM0405'
               WHEN  'DCOM0408'
               WHEN  'DCOM0411'
                      IF  COMU-OPER-UN       IS NUMERIC     AND
                          COMU-OPER-UN       GREATER ZEROS
                          MOVE COMU-OPCAO-UN     TO  OUT-OPCAO-N
                          MOVE COMU-OPER-UN(1:4) TO  OUT-OPER-N
                          MOVE COMU-OPER-UN(5:9) TO  OUT-NSEQ-N
                          MOVE WRK-49369         TO  OUT-OPER-ATTR
                                                     OUT-NSEQ-ATTR
                      END-IF

               WHEN  'DCOM0403'
               WHEN  'DCOM0406'
               WHEN  'DCOM0409'
                      IF  COMU-AGENCIA-UN    IS NUMERIC    AND
                          COMU-AGENCIA-UN    GREATER ZEROS
                          MOVE COMU-OPCAO-UN     TO  OUT-OPCAO-N
                          MOVE COMU-AGENCIA-UN   TO  OUT-AGENCIA-N
                          MOVE WRK-49369         TO  OUT-AGENCIA-ATTR
                      END-IF

               WHEN  'DCOM0404'
               WHEN  'DCOM0407'
               WHEN  'DCOM0410'
                      IF  COMU-CONTA-UN      IS NUMERIC    AND
                          COMU-CONTA-UN      GREATER ZEROS
                          MOVE COMU-OPCAO-UN     TO  OUT-OPCAO-N
                          MOVE COMU-AGENC-UN     TO  OUT-AGENC-N
                          MOVE COMU-CONTA-UN     TO  OUT-CONTA-N
                          MOVE WRK-49369         TO  OUT-AGENC-ATTR
                      ELSE
FS2511*                  XIF  COMU-CNPJ-UN   IS NUMERIC    AND
FS2511*                       COMU-CNPJ-UN   GREATER ZEROS
FS2511                    IF  COMU-CNPJ-UN   NOT EQUAL SPACES AND
FS2511                        COMU-CNPJ-UN   NOT EQUAL LOW-VALUES
                              MOVE COMU-OPCAO-UN TO  OUT-OPCAO-N
                              MOVE COMU-CNPJ-UN  TO  OUT-CNPJ-N
                              MOVE COMU-FILIAL-UN
                                                 TO OUT-FILIAL-N
                              MOVE COMU-DIG-UN   TO  OUT-DIG-N
                              MOVE WRK-49369     TO  OUT-CNPJ-ATTR
                                                     OUT-FILIAL-ATTR
                                                     OUT-DIG-ATTR
                          END-IF
                      END-IF
             END-EVALUATE

               IF  COMU-MENSAGEM-UN    NOT EQUAL SPACES
                   MOVE COMU-MENSAGEM-UN
                                       TO OUT-MENSAG
                   GO TO 1100-99-FIM
               END-IF
           ELSE
               MOVE WRK-MENSAGEM       TO  COMU-AREA
               MOVE COMU-OPCAO         TO  OUT-TPOOPC
               MOVE COMU-TIMESTAMP     TO  OUT-TMSTAMP
               MOVE COMU-SENHAS        TO  OUT-SENHAS
                                           INP-SENHAS
               MOVE COMU-COMANDO       TO  OUT-COMANDO
           END-IF.

           MOVE 'SELECIONE A OPCAO DESEJADA E TECLE <ENTER>'
                                       TO  OUT-MENSAG.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-DCOMDBTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  INPUT-DBTM.

           PERFORM  2100-DEVOLVER-TELA.

           EVALUATE  TRUE
               WHEN  INP-PFK          EQUAL  'H'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK01')
                     MOVE  INP-MENSAG     TO  OUT-MENSAG
                     PERFORM  2100-DEVOLVER-TELA

               WHEN  INP-PFK          EQUAL  '3'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL 'PFK03')
                     PERFORM  2200-RETORNAR-CHAMADOR

               WHEN  INP-PFK          EQUAL  '5'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK05')
                     PERFORM  2300-MENU-ROTINAS

               WHEN  INP-PFK          EQUAL  'A'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK10')
                     PERFORM  2400-MENU-DCOM

               WHEN  INP-PFK          EQUAL '.' AND
                     INP-COMANDO      EQUAL  SPACES
                     PERFORM  2500-TRATAR-ENTER

               WHEN  OTHER
                     MOVE 'PF INVALIDA'   TO OUT-MENSAG
                     PERFORM  2100-DEVOLVER-TELA
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2515-PASSAR-INSPECT.

           MOVE  INP-TPOOPC            TO  OUT-TPOOPC
           MOVE  INP-TMSTAMP           TO  OUT-TMSTAMP
           MOVE  INP-OPCAO             TO  OUT-OPCAO
           MOVE  INP-OPER              TO  OUT-OPER
           MOVE  INP-NSEQ              TO  OUT-NSEQ
           MOVE  INP-AGENCIA           TO  OUT-AGENCIA
           MOVE  INP-AGENC             TO  OUT-AGENC
           MOVE  INP-CONTA             TO  OUT-CONTA
           MOVE  INP-CNPJ              TO  OUT-CNPJ
           MOVE  INP-FILIAL            TO  OUT-FILIAL
           MOVE  INP-DIG               TO  OUT-DIG.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA.

           MOVE LENGTH OF COMU-AREA    TO COMU-LL
           MOVE ZEROS                  TO COMU-ZZ

           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM0401'             TO WRK-TELA
           MOVE 'DCOM0402'             TO COMU-TRANSACAO
           MOVE '3'                    TO COMU-PFK1
           MOVE INP-SENHAS             TO COMU-SENHAS
           MOVE COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.
           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0402'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  '5'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.

           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0402'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  'A'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-INCONSIS

           PERFORM 2510-CONSISTENCIA-FISICA.

           IF  WRK-INCONSIS            EQUAL SPACES
121208         PERFORM 2550-VER-DISPONIBILIDADE

               IF  WRK-INCONSIS        EQUAL SPACES
                   EVALUATE INP-OPCAO
                       WHEN '1'
                            PERFORM 2520-TRATAR-LIBERAR

                       WHEN '2'
                            PERFORM 2530-TRATAR-CANCELAR

                       WHEN '3'
                            PERFORM 2540-TRATAR-CONSULTA
BRQ001
BRQ001                 WHEN '4'
BRQ001                      PERFORM 2560-TRATAR-CONSULTA-PENDENTES

                       WHEN OTHER
                            MOVE WRK-49369 TO OUT-OPCAO-ATTR
                            MOVE  'OPCAO INVALIDA'
                                           TO OUT-MENSAG
                   END-EVALUATE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           IF  INP-OPCAO      NOT EQUAL  SPACES AND LOW-VALUES
               IF (INP-OPCAO  NOT NUMERIC)         OR
                  (INP-OPCAO  EQUAL ZEROS)         OR
BRQ001            (INP-OPCAO-N NOT EQUAL 1 AND 2 AND 3 AND 4)
                  MOVE 'OPCAO INVALIDA'
                                          TO  OUT-MENSAG
                  MOVE '*'                TO  WRK-INCONSIS
                  MOVE WRK-49369          TO  OUT-OPCAO-ATTR
                  GO                      TO  2510-99-FIM
               END-IF
           ELSE
               MOVE 'OPCAO INVALIDA'    TO  OUT-MENSAG
               MOVE '*'                 TO  WRK-INCONSIS
               MOVE WRK-49369           TO  OUT-OPCAO-ATTR
               GO                       TO  2510-99-FIM
           END-IF.

           IF  (INP-OPER           EQUAL  SPACES OR LOW-VALUES )  AND
               (INP-AGENCIA        EQUAL  SPACES OR LOW-VALUES )  AND
               (INP-AGENC          EQUAL  SPACES OR LOW-VALUES )  AND
               (INP-CONTA          EQUAL  SPACES OR LOW-VALUES )  AND
               (INP-CNPJ           EQUAL  SPACES OR LOW-VALUES )
                MOVE 'DIGITAR: <OPERACAO> OU <AGENCIA> OU <AGENCIA E CON
      -              'TA> OU <CNPJ/CPF>'  TO  OUT-MENSAG
                MOVE '*'                  TO  WRK-INCONSIS
                MOVE WRK-49361            TO  OUT-OPER-ATTR
                GO                        TO  2510-99-FIM
           END-IF.

           IF (INP-OPER        NOT EQUAL SPACES AND  LOW-VALUES)
           OR (INP-NSEQ        NOT EQUAL SPACES AND  LOW-VALUES)
               IF (INP-OPER        NOT NUMERIC)  OR
                  (INP-OPER-N      EQUAL ZEROS) OR
                  (INP-NSEQ        NOT NUMERIC)  OR
                  (INP-NSEQ-N      EQUAL ZEROS)
                  MOVE 'OPERACAO INVALIDA'
                                          TO  OUT-MENSAG
                  MOVE '*'                TO  WRK-INCONSIS
                  MOVE WRK-49369          TO  OUT-OPER-ATTR
                                              OUT-NSEQ-ATTR
                  GO                      TO  2510-99-FIM
               END-IF
           END-IF.

           IF  INP-AGENCIA            NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-AGENCIA        NOT NUMERIC)  OR
                  (INP-AGENCIA-N      EQUAL ZEROS)
                  MOVE 'AGENCIA INVALIDA'
                                          TO  OUT-MENSAG
                  MOVE '*'                TO  WRK-INCONSIS
                  MOVE WRK-49369          TO  OUT-AGENCIA-ATTR
                  GO                      TO  2510-99-FIM
               END-IF

               MOVE INP-AGENCIA-N         TO  WRK-AGENCIA-TELA

               PERFORM 2512-ACESSAR-DCOM6416
               IF  WRK-INCONSIS           EQUAL '*'
                  MOVE WRK-49369          TO OUT-AGENCIA-ATTR
                  GO                      TO  2510-99-FIM
               END-IF

               PERFORM 2511-VERIFICAR-PERFIL
               IF  WRK-INCONSIS           EQUAL '*'
                  MOVE WRK-49369          TO OUT-AGENCIA-ATTR
                  GO                      TO  2510-99-FIM
               END-IF

           END-IF.

           IF  INP-CNPJ             NOT EQUAL SPACES AND 
               INP-CNPJ             NOT EQUAL LOW-VALUES
FS2511         MOVE 'CPF/CNPJ INVALIDO' TO OUT-MENSAG
FS2511         MOVE '*' TO WRK-INCONSIS
FS2511         MOVE WRK-49369 TO OUT-DIG-ATTR
FS2511                           OUT-FILIAL-ATTR
FS2511                           OUT-CNPJ-ATTR
FS2511         GO TO 2510-99-FIM
FS2511     END-IF.

FS2511     IF  INP-FILIAL EQUAL SPACES OR LOW-VALUES
FS2511         MOVE 'CPF/CNPJ INVALIDO' TO OUT-MENSAG
FS2511         MOVE '*' TO WRK-INCONSIS
FS2511         MOVE WRK-49369 TO OUT-DIG-ATTR
FS2511                           OUT-FILIAL-ATTR
FS2511                           OUT-CNPJ-ATTR
FS2511         GO TO 2510-99-FIM
FS2511     END-IF.

           IF  INP-DIG              NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-DIG          NOT NUMERIC)  OR
                  (INP-DIG-N        EQUAL ZEROS)
                  MOVE 'CPF/CNPJ INVALIDO'
                                          TO  OUT-MENSAG
                  MOVE '*'                TO  WRK-INCONSIS
                  MOVE WRK-49369          TO  OUT-DIG-ATTR
                                              OUT-FILIAL-ATTR
                                              OUT-CNPJ-ATTR
                  GO                      TO  2510-99-FIM
               END-IF
           END-IF.

FS2511*    MOVE INP-CNPJ                  TO  WRK-NUMERO.
FS2511     MOVE INP-CNPJ-N                TO  WRK-NUMERO.

FS2511*    IF   (INP-FILIAL          EQUAL SPACES OR LOW-VALUES) OR
FS2511*         (INP-FILIAL          NOT NUMERIC)
FS2511*         MOVE ZEROS           TO  WRK-FILIAL INP-FILIAL
FS2511*    ELSE
FS2511*         MOVE INP-FILIAL-N    TO  WRK-FILIAL
FS2511*    END-IF.
FS2511     MOVE INP-FILIAL-N              TO  WRK-FILIAL.

FS2511*    MOVE SPACES               TO  WRK-CONTROLE.
FS2511     MOVE ZEROS                TO  WRK-CONTROLE.

FS2511*    CALL 'BRAD0110' USING    WRK-CPFCGC
FS2511*                             WRK-CONTROLE.

FS2511     CALL 'BRAD2000' USING    WRK-CPFCGC
FS2511                              WRK-CONTROLE.

FS2511*    IF  RETURN-CODE         EQUAL  12
FS2511*        MOVE  '*'            TO  WRK-INCONSIS
FS2511*        MOVE  WRK-49369      TO  OUT-CNPJ-ATTR
FS2511*        MOVE  'CPF/CNPJ VICIADO' TO OUT-MENSAG
FS2511*        GO                   TO  2510-99-FIM
FS2511*    END-IF.

FS2511     IF  RETURN-CODE NOT EQUAL ZEROS
FS2511         MOVE  '*'            TO  WRK-INCONSIS
FS2511         MOVE  WRK-49369      TO  OUT-CNPJ-ATTR
FS2511         MOVE  'CPF/CNPJ VICIADO' TO OUT-MENSAG
FS2511         GO                   TO  2510-99-FIM
FS2511     END-IF.

FS2511     IF  INP-DIG-N       NOT EQUAL WRK-CONTROLE
               MOVE  '*'            TO   WRK-INCONSIS
               MOVE  WRK-49369      TO   OUT-DIG-ATTR
               MOVE  'CONTROLE INVALIDO' TO OUT-MENSAG
           END-IF.

           IF  INP-CNPJ             NOT EQUAL SPACES AND  LOW-VALUES
               PERFORM 2513-ACESSAR-DCOM6408
               IF  WRK-INCONSIS     EQUAL '*'
                   GO TO 2510-99-FIM
               END-IF
           END-IF.

           IF  INP-AGENC            NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-AGENC        NOT NUMERIC)  OR
                  (INP-AGENC-N      EQUAL ZEROS)
                  MOVE 'AGENCIA INVALIDA'
                                          TO  OUT-MENSAG
                  MOVE '*'                TO  WRK-INCONSIS
                  MOVE WRK-49369          TO  OUT-AGENC-ATTR
                  GO                      TO  2510-99-FIM
               END-IF
           END-IF.

           IF  INP-CONTA            NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-CONTA        NOT NUMERIC)  OR
                  (INP-CONTA-N      EQUAL ZEROS)
                  MOVE 'CONTA INVALIDA'
                                          TO  OUT-MENSAG
                  MOVE '*'                TO  WRK-INCONSIS
                  MOVE WRK-49369          TO  OUT-CONTA-ATTR
                  GO                      TO  2510-99-FIM
               END-IF
           END-IF.

           IF  INP-AGENC            NOT EQUAL SPACES AND  LOW-VALUES
           OR  INP-CONTA            NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-CONTA        NOT NUMERIC)  OR
                  (INP-CONTA-N      EQUAL ZEROS)  OR
                  (INP-AGENC        NOT NUMERIC)  OR
                  (INP-AGENC-N      EQUAL ZEROS)
                  MOVE 'AGENCIA/CONTA INVALIDOS'
                                          TO  OUT-MENSAG
                  MOVE '*'                TO  WRK-INCONSIS
                  MOVE WRK-49369          TO  OUT-AGENC-ATTR
                  MOVE WRK-49369          TO  OUT-CONTA-ATTR
                  GO                      TO  2510-99-FIM
               END-IF
           END-IF.

           IF  INP-AGENC            NOT EQUAL SPACES AND  LOW-VALUES
               PERFORM 2513-ACESSAR-DCOM6408
               IF  WRK-INCONSIS     EQUAL '*'
                   GO TO 2510-99-FIM
               END-IF
               MOVE INP-AGENC-N           TO  WRK-AGENCIA-TELA
               PERFORM 2511-VERIFICAR-PERFIL
               IF  WRK-INCONSIS           EQUAL '*'
                  MOVE WRK-49369          TO  OUT-AGENC-ATTR
                  MOVE WRK-49369          TO  OUT-CONTA-ATTR
                  GO                      TO  2510-99-FIM
               END-IF
           END-IF.

           MOVE ZEROS                    TO  WRK-QTDE-SELECAO.

           IF  (INP-OPER       NOT EQUAL  SPACES AND LOW-VALUES)
                ADD  1                    TO  WRK-QTDE-SELECAO
           END-IF.

           IF  (INP-AGENCIA    NOT EQUAL  SPACES AND LOW-VALUES)
                ADD  1                    TO  WRK-QTDE-SELECAO
           END-IF.

           IF  (INP-AGENC      NOT EQUAL  SPACES AND LOW-VALUES)
                ADD  1                    TO  WRK-QTDE-SELECAO
           END-IF.

           IF  (INP-CNPJ       NOT EQUAL  SPACES AND LOW-VALUES)
                ADD  1                    TO  WRK-QTDE-SELECAO
           END-IF.

           IF  (WRK-QTDE-SELECAO EQUAL  ZEROS OR  GREATER  1)
                MOVE WRK-49369            TO  OUT-OPER-ATTR
                                              OUT-NSEQ-ATTR
                                              OUT-AGENCIA-ATTR
                                              OUT-AGENC-ATTR
                                              OUT-CONTA-ATTR
                                              OUT-CNPJ-ATTR
                MOVE 'DIGITAR: <OPERACAO> OU <AGENCIA> OU <AGENCIA E CON
      -              'TA> OU <CNPJ/CPF>'  TO  OUT-MENSAG
                MOVE '*'                  TO  WRK-INCONSIS
                GO                        TO  2510-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2511-VERIFICAR-PERFIL           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           MOVE WRK-COD-USER-R          TO ROTENT-6437-CFUNC-ENV.
           MOVE WRK-COD-DEPTO-N         TO WRK-AGENCIA-NUM
           MOVE WRK-AGENCIA-NUM         TO ROTENT-6437-AGEUSU-ENV.
           MOVE LNK-IO-LTERM            TO ROTENT-6437-CTERM.
           MOVE WRK-AGENCIA-TELA        TO ROTENT-6437-AGEPESQ-ENV.
           MOVE 'A'                     TO ROTENT-6437-FUNCAO-ENV.
           MOVE 'DCOM6437'              TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6437-ENVIO
                                             ROTSAI-6437-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET     NOT EQUAL ZEROS
           OR  ROTSAI-6437-RESUL-RET   GREATER 3
               MOVE '*'                TO WRK-INCONSIS
               IF  ROTSAI-6437-RESUL-RET  GREATER 3
                   MOVE 'USUARIO NAO AUTORIZADO'
                                       TO OUT-MENSAG
               ELSE
                   MOVE ROTSAI-6437-MENS-RET
                                       TO OUT-MENSAG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2512-ACESSAR-DCOM6416           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-6416-ENTRADA-ROTEADOR
                      WRK-6416-SAIDA-ROTEADOR.

           MOVE WRK-COD-USER-R         TO WRK-6416E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-6416E-CTERM
           MOVE 237                    TO WRK-6416E-BANCO
           MOVE WRK-AGENCIA-TELA       TO WRK-6416E-DEPDC

           MOVE 'DCOM6416'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-6416-ENTRADA-ROTEADOR
                                             WRK-6416-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'         TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE WRK-6416S-MSG-RETORNO
                                        TO OUT-MENSAG
                     MOVE '*'           TO WRK-INCONSIS

           END-EVALUATE.

      *----------------------------------------------------------------*
       2512-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2513-ACESSAR-DCOM6408           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6408-AREA
                      ROTSAI-6408-RETORNO.

           MOVE WRK-COD-USER-R         TO ROTENT-6408-CFUNC-ENV
           MOVE LNK-IO-LTERM           TO ROTENT-6408-CTERM-ENV
           IF  INP-CNPJ            NOT EQUAL SPACES AND  LOW-VALUES
               MOVE INP-CNPJ-N         TO ROTENT-6408-NUM-CNPJ-ENV
               IF  INP-FILIAL          EQUAL SPACES OR LOW-VALUES
                   MOVE ZEROS          TO ROTENT-6408-FIL-CNPJ-ENV
               ELSE
                   MOVE INP-FILIAL-N   TO ROTENT-6408-FIL-CNPJ-ENV
               END-IF
               MOVE INP-DIG-N          TO ROTENT-6408-CTR-CNPJ-ENV
           ELSE
               MOVE INP-AGENC-N        TO ROTENT-6408-CJUNC-ENV
               MOVE INP-CONTA-N        TO ROTENT-6408-CCTAC-ENV

      ***** ACESSO BRAD0431    *****
               MOVE INP-CONTA-N        TO WRK-CONTA-0431
               MOVE 07                 TO WRK-TAMANHO-0431
               MOVE SPACES             TO WRK-DIGITO-0431

               CALL 'BRAD0431'         USING WRK-CONTA-0431
                                             WRK-DIGITO-0431
                                             WRK-TAMANHO-0431
               IF  WRK-DIGITO-0431     EQUAL '.'
                   MOVE  'ERRO NO ACESSO A BRAD0431'
                                       TO ERR-TEXTO
                   MOVE 'APL'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
                   IF  WRK-DIGITO-0431 EQUAL 'P'
                       MOVE '0'        TO WRK-DIGITO-0431
                   END-IF
               END-IF
               MOVE WRK-DIGITO-0431    TO ROTENT-6408-DCTAC-ENV
           END-IF.

           MOVE 'DCOM6408'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6408-AREA
                                             ROTSAI-6408-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  ROTSAI-6408-COD-RET
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE '*'            TO WRK-INCONSIS
                 IF  ROTSAI-6408-COD-RET EQUAL '0003'
                     IF  INP-CNPJ NOT EQUAL SPACES AND LOW-VALUES
                        MOVE 'CPF/CNPJ INEXISTENTE'
                                         TO OUT-MENSAG
                        MOVE WRK-49369   TO OUT-DIG-ATTR
                                            OUT-FILIAL-ATTR
                                            OUT-CNPJ-ATTR
                     ELSE
                        MOVE 'AGENCIA/CONTA INEXISTENTE'
                                         TO OUT-MENSAG
                        MOVE WRK-49369   TO OUT-AGENC-ATTR
                                            OUT-CONTA-ATTR
                     END-IF
                 ELSE
                     MOVE ROTSAI-6408-MENS-RET
                                         TO OUT-MENSAG
                 END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2513-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2515-PASSAR-INSPECT             SECTION.
      *----------------------------------------------------------------*

           IF INP-OPER              NOT EQUAL SPACES AND LOW-VALUES
              INSPECT INP-OPER-N        REPLACING LEADING ' ' BY '0'
           END-IF.

           IF INP-NSEQ              NOT EQUAL SPACES AND LOW-VALUES
              INSPECT INP-NSEQ-N        REPLACING LEADING ' ' BY '0'
           END-IF.

           IF INP-AGENCIA           NOT EQUAL SPACES AND LOW-VALUES
              INSPECT INP-AGENCIA-N     REPLACING LEADING ' ' BY '0'
           END-IF.

           IF INP-AGENC             NOT EQUAL SPACES AND LOW-VALUES
              INSPECT INP-AGENC-N       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF INP-CONTA             NOT EQUAL SPACES AND LOW-VALUES
              INSPECT INP-CONTA-N       REPLACING LEADING ' ' BY '0'
           END-IF.

FS2511*         IF INP-CNPJ              NOT EQUAL SPACES AND LOW-VALUES
FS2511*            INSPECT INP-CNPJ-N        REPLACING LEADING ' ' BY '0'
FS2511*       END-IF.

FS2511*         IF INP-FILIAL            NOT EQUAL SPACES AND LOW-VALUES
FS2511*            INSPECT INP-FILIAL-N      REPLACING LEADING ' ' BY '0'
FS2511*       END-IF.

           IF INP-DIG               NOT EQUAL SPACES AND LOW-VALUES
              INSPECT INP-DIG-N         REPLACING LEADING ' ' BY '0'
           END-IF.

      *----------------------------------------------------------------*
       2515-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2520-TRATAR-LIBERAR             SECTION.
      *----------------------------------------------------------------*

           IF INP-OPER             NOT EQUAL  SPACES
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0405'         TO  WRK-TELA
              GO                       TO  2520-99-FIM
           END-IF.

           IF INP-AGENCIA          NOT EQUAL  SPACES
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0403'         TO  WRK-TELA
              GO                       TO  2520-99-FIM
           END-IF.

           IF (INP-AGENC           NOT EQUAL  SPACES) AND
              (INP-CONTA           NOT EQUAL  SPACES) OR
              (INP-CNPJ            NOT EQUAL  SPACES)
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0404'         TO  WRK-TELA
              GO                       TO  2520-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2521-PASSAR-CONTROLE-DIVS       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     COMU-AREA-UN.

           MOVE  +294                  TO COMU-LL-UN.
           MOVE  ZEROS                 TO COMU-ZZ-UN.
           MOVE  INP-COMANDO           TO COMU-COMANDO-UN
           MOVE  INP-SENHAS            TO COMU-SENHAS-UN
           MOVE  INP-OPCAO             TO COMU-OPCAO-UN
           MOVE  INP-AGENCIA           TO COMU-AGENCIA-UN
           MOVE  INP-AGENC             TO COMU-AGENC-UN
           MOVE  INP-CONTA             TO COMU-CONTA-UN
           MOVE  INP-CNPJ              TO COMU-CNPJ-UN
           MOVE  INP-FILIAL            TO COMU-FILIAL-UN
           MOVE  INP-DIG               TO COMU-DIG-UN
           MOVE  INP-TMSTAMP           TO COMU-TIMESTAMP-UN
           MOVE  INP-OPER-N            TO COMU-DANO-OPER-UN
           MOVE  INP-NSEQ-N            TO COMU-NSEQ-OPER-UN
           MOVE  'DCOM0402'            TO COMU-TRANSACAO-UN.
           MOVE  COMU-AREA-UN          TO WRK-MENSAGEM.
           MOVE  WRK-CHNG              TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2521-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2530-TRATAR-CANCELAR            SECTION.
      *----------------------------------------------------------------*

           IF INP-OPER             NOT EQUAL  SPACES
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0408'         TO  WRK-TELA
              GO                       TO  2530-99-FIM
           END-IF.

           IF INP-AGENCIA          NOT EQUAL  SPACES
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0406'         TO  WRK-TELA
              GO                       TO  2530-99-FIM
           END-IF.

           IF (INP-AGENC           NOT EQUAL  SPACES) AND
              (INP-CONTA           NOT EQUAL  SPACES) OR
              (INP-CNPJ            NOT EQUAL  SPACES)
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0407'         TO  WRK-TELA
              GO                       TO  2530-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2540-TRATAR-CONSULTA            SECTION.
      *----------------------------------------------------------------*

           IF INP-OPER             NOT EQUAL  SPACES
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0411'         TO  WRK-TELA
              GO                       TO  2540-99-FIM
           END-IF.

           IF INP-AGENCIA          NOT EQUAL  SPACES
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0409'         TO  WRK-TELA
              GO                       TO  2540-99-FIM
           END-IF.

           IF (INP-AGENC           NOT EQUAL  SPACES) AND
              (INP-CONTA           NOT EQUAL  SPACES) OR
              (INP-CNPJ            NOT EQUAL  SPACES)
              PERFORM 2521-PASSAR-CONTROLE-DIVS
              MOVE  'DCOM0410'         TO  WRK-TELA
              GO                       TO  2540-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2550-VER-DISPONIBILIDADE        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6438-ENVIO.

           MOVE 'N'                    TO ROTENT-6438-FLAG.
           MOVE WRK-COD-USER           TO ROTENT-6438-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6438-CTERM.
           MOVE 2                      TO ROTENT-6438-AGEUSU-ENV.
           MOVE ZEROS                  TO ROTENT-6438-PROD-ENV.
           MOVE 'M'                    TO ROTENT-6438-PROC-ENV.

           EVALUATE  INP-OPCAO
               WHEN '1'
                     MOVE 'I'          TO ROTENT-6438-FUNC-ENV

               WHEN '2'
                     MOVE 'E'          TO ROTENT-6438-FUNC-ENV

               WHEN '3'
               WHEN '4'
                     MOVE 'C'          TO ROTENT-6438-FUNC-ENV
           END-EVALUATE.

      *--> LIBERACAO DA OPERACAO
           MOVE 008                    TO ROTENT-6438-OPCAO-ENV
           MOVE 'OP'                   TO ROTENT-6438-MOT-ENV.
           MOVE 'DCOM6438'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6438-ENVIO
                                             ROTSAI-6438-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6438-COD-RET     EQUAL '0099'
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  ROTSAI-6438-COD-RET     EQUAL ZEROS AND
               ROTSAI-6438-RESUL-RET   EQUAL 1
               NEXT SENTENCE
           ELSE
               MOVE '*'                TO WRK-INCONSIS
               MOVE ROTSAI-6438-MENS-RET
                                       TO OUT-MENSAG
           END-IF.

      *----------------------------------------------------------------*
       2550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BRQ001*----------------------------------------------------------------*
BRQ001 2560-TRATAR-CONSULTA-PENDENTES  SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     IF INP-OPER             NOT EQUAL  SPACES
BRQ001        PERFORM 2521-PASSAR-CONTROLE-DIVS
BRQ001        MOVE  'DCOM0411'         TO  WRK-TELA
BRQ001        GO                       TO  2560-99-FIM
BRQ001     END-IF.
BRQ001
BRQ001     IF INP-AGENCIA          NOT EQUAL  SPACES
BRQ001        PERFORM 2521-PASSAR-CONTROLE-DIVS
BRQ001        MOVE  'DCOM0409'         TO  WRK-TELA
BRQ001        GO                       TO  2560-99-FIM
BRQ001     END-IF.
BRQ001
BRQ001     IF (INP-AGENC           NOT EQUAL  SPACES) AND
BRQ001        (INP-CONTA           NOT EQUAL  SPACES) OR
BRQ001        (INP-CNPJ            NOT EQUAL  SPACES)
BRQ001        PERFORM 2521-PASSAR-CONTROLE-DIVS
BRQ001        MOVE  'DCOM0410'         TO  WRK-TELA
BRQ001        GO                       TO  2560-99-FIM
BRQ001     END-IF.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 2560-99-FIM.                    EXIT.
BRQ001*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD0660'            USING  OUTPUT-DBTM
                                              660-DCOMDBTM.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO  ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO  WRK-RETURN-CODE
              MOVE  10                 TO  WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO  ERR-TEXTO
              PERFORM  9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO  WRK-FUNCAO.
           MOVE  OUTPUT-DBTM           TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO      NOT EQUAL  SPACES  AND  LOW-VALUES
              MOVE  'DCOM0402'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'DCOM0402'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER          TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL  'BRAD7100'        USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
