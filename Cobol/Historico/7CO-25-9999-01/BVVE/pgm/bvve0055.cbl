000010*******************************************************
000020 IDENTIFICATION DIVISION.
000030*******************************************************
000040 PROGRAM-ID. BVVE0055.
000050*-----------------------------------------------------*
000060*           C P M    -   S I S T E M A S              *
000070*=====================================================*
000080*                                                     *
000090*  PROGRAMA     :   BVVE0055                          *
000100*  PROGRAMADOR  :   MIRELLA      -   CPM              *
000110*  SUPERVISOR   :   DENYLSON     -   CPM              *
000120*  ANALISTA     :   SHEYLA       -   GR. 91           *
000130*  DATA         :   JULHO        /   1997             *
000140*                                                     *
000150*=====================================================*
000160*                                                     *
000170*   INPUT       :  ARQPFEB   -  I#BVVEVP - LRECL=0500 *
000180*                                                     *
000190*   OUTPUT      :  SELPFEB   -  I#BVVEVR - LRECL=0200 *
000210*-----------------------------------------------------*
000220*                    OBJETIVO                         *
000230*-----------------------------------------------------*
000240*                                                     *
000250*    - SELECIONAR ALGUNS REGISTROS DO ARQUIVO         *
000260*    ARQPFEB E GRAVAR EM SELPFEB.                     *
000270*                                                     *
000280*=====================================================*
000290*******************************************************
000300 ENVIRONMENT DIVISION.
000310*******************************************************
000320*-----------------------------------------------------*
000330 CONFIGURATION SECTION.
000340*-----------------------------------------------------*
000350
000360 SPECIAL-NAMES.
000370     DECIMAL-POINT IS COMMA.
000380
000390*-----------------------------------------------------*
000400 INPUT-OUTPUT SECTION.
000420
000430 FILE-CONTROL.
000440
000450     SELECT   ARQPFEB      ASSIGN  TO  UT-S-ARQPFEB
000460              FILE         STATUS  IS  WRK-FS-ARQPFEB.
000470     SELECT   SELPFEB      ASSIGN  TO  UT-S-SELPFEB
000480              FILE         STATUS  IS  WRK-FS-SELPFEB.
000490
000500*******************************************************
000510 DATA DIVISION.
000520*******************************************************
000530*-----------------------------------------------------*
000540 FILE SECTION.
000550*-----------------------------------------------------*
000560
000570 FD  ARQPFEB
000580     LABEL RECORD STANDARD
000590     RECORDING MODE F
000600     BLOCK 0.
000610
000630
000640 FD  SELPFEB
000650     LABEL RECORD STANDARD
000660     RECORDING MODE F
000670     BLOCK 0.
000680
000690 01  REG-SELPFEB                 PIC  X(0200).
000700
000710*-----------------------------------------------------*
000720 WORKING-STORAGE SECTION.
000730*-----------------------------------------------------*
000740
000750 77  FILLER                      PIC  X(30)  VALUE
000760     'INICIO DA WORKING'.
000770
000780 77  WRK-ABEND                   PIC S9(04)  VALUE +1111 COMP.
000790 77  WRK-FS-ARQPFEB              PIC  X(02)  VALUE SPACES.
000800 77  WRK-FS-SELPFEB              PIC  X(02)  VALUE SPACES.
000810
000820 77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.
000840 77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.
000850 77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.
000860 77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
000870
000880*-----------------------------------------------------*
000890*   %%%%        DEFINICAO DOS ARQUIVOS         %%%%   *
000900*-----------------------------------------------------*
000910* ARQPFEB - LRECL=0500 - INPUT
-INC I#BVVEVP
000930
000940
000950* SELPFEB - LRECL=0200 - OUTPUT
-INC I#BVVEVR
000970
000980*-----------------------------------------------------*
000990*   %%%%%     TABELA INTERNA DE MENSAGENS     %%%%%   *
001000*-----------------------------------------------------*
-INC I#PFEB12
001020
001030*******************************************************
001050*******************************************************
001060*-----------------------------------------------------*
001070 00000-INICIO  SECTION.
001080*-----------------------------------------------------*
001090
001100     OPEN    INPUT  ARQPFEB
001110             OUTPUT SELPFEB
001120
001130     MOVE    WRK-ABERTURA        TO        WRK-OPERACAO
001140     PERFORM 0500-TESTAR-FS
001150
001160     PERFORM 0100-INICIALIZA
001170
001180     PERFORM 0300-PROCESSA UNTIL WRK-FS-ARQPFEB EQUAL +10
001190
001200     CLOSE   ARQPFEB SELPFEB
001210
001220     MOVE    WRK-FECHAMENTO      TO        WRK-OPERACAO
001230     PERFORM 0500-TESTAR-FS
001240
001260
001270*-----------------------------------------------------*
001280 00000-INICIO-FIM. EXIT.
001290*-----------------------------------------------------*
001300*-----------------------------------------------------*
001310 0100-INICIALIZA SECTION.
001320*-----------------------------------------------------*
001330
001340     PERFORM 0200-LER-ARQPFEB
001350
001360     IF  WRK-FS-ARQPFEB      EQUAL        +10
001370         DISPLAY '************** BVVE0055 **************'
001380         DISPLAY '*           SR. OPERADOR,            *'
001390         DISPLAY '*    O ARQUIVO BVVE0055 ESTA VAZIO.  *'
001400         DISPLAY '*   O PROCESSAMENTO SERA ENCERRADO.  *'
001410         DISPLAY '************** BVVE0055 **************'.
001420
001430*-----------------------------------------------------*
001440 0100-INICIALIZA-FIM. EXIT.
001450*-----------------------------------------------------*
001470 0200-LER-ARQPFEB SECTION.
001480*-----------------------------------------------------*
001490
001500     READ ARQPFEB INTO PGTO-REG
001510
001520     IF  WRK-FS-ARQPFEB      EQUAL         +10
001530
001540         GO TO 0200-LER-ARQPFEB-FIM.
001550
001560     MOVE    WRK-LEITURA         TO        WRK-OPERACAO
001570     PERFORM 0600-TESTAR-FS-ARQPFEB.
001580
001590*-----------------------------------------------------*
001600 0200-LER-ARQPFEB-FIM. EXIT.
001610*-----------------------------------------------------*
001620*-----------------------------------------------------*
001630 0300-PROCESSA SECTION.
001640*-----------------------------------------------------*
001650
001660     IF (PGTO-COD-REG         EQUAL        1)            AND
001680
001690         PERFORM  0350-PESQUISA-MENSAGEM
001700
001710         PERFORM  0400-GRAVA-SELPFEB.
001720
001730     PERFORM  0200-LER-ARQPFEB.
001740
001750*-----------------------------------------------------*
001760 0300-PROCESSA-FIM. EXIT.
001770*-----------------------------------------------------*
001780*-----------------------------------------------------*
001790 0350-PESQUISA-MENSAGEM SECTION.
001800*-----------------------------------------------------*
001810
001820     PERFORM  0355-PESQUISANDO VARYING WRK-IND-MENS FROM
001830              1 BY 1 UNTIL WRK-IND-MENS GREATER  100   OR
001840              TAB-COD-MENS(WRK-IND-MENS) EQUAL PGTO-INF-RET1-TRAN
001850
001860     IF   WRK-IND-MENS      GREATER        100
001870
001890
001900     ELSE
001910           MOVE  TAB-MENSAGEM(WRK-IND-MENS)
001920                                 TO        BVVEPFEB-MENSAGEM-S.
001930
001940*-----------------------------------------------------*
001950 0350-PESQUISA-MENSAGEM-FIM. EXIT.
001960*-----------------------------------------------------*
001970*-----------------------------------------------------*
001980 0355-PESQUISANDO SECTION.
001990*-----------------------------------------------------*
002000*-----------------------------------------------------*
002010 0355-PESQUISANDO-FIM. EXIT.
002020*-----------------------------------------------------*
002030*-----------------------------------------------------*
002040 0400-GRAVA-SELPFEB SECTION.
002050*-----------------------------------------------------*
002060
002070     MOVE   'PFEB'               TO        BVVEPFEB-ROTINA-S
002080     MOVE   PGTO-AGEN-TRAN       TO        BVVEPFEB-AGENCIA-S
002100     MOVE   PGTO-DIGCC-TRAN      TO        BVVEPFEB-DIGC-S
002110     MOVE   PGTO-NOME-TRAN       TO        BVVEPFEB-NOME-S
002120     MOVE   PGTO-CGC-NUM-TRAN    TO        BVVEPFEB-CGC-NUM-S
002130     MOVE   PGTO-CGC-FIL-TRAN    TO        BVVEPFEB-CGC-FIL-S
002140     MOVE   PGTO-CGC-DIG-TRAN    TO        BVVEPFEB-CGC-CTR-S
002150     MOVE   PGTO-DTEFPG-TRAN     TO        BVVEPFEB-DT-CRED-S
002160     MOVE   PGTO-VLPAG-TRAN      TO        BVVEPFEB-VR-TRAN-S
002170     MOVE   PGTO-COD-BCO-TRAN    TO        BVVEPFEB-COD-BCO-S
002180     MOVE   SPACES               TO        BVVEPFEB-RESERVA-S
002190
002200     WRITE  REG-SELPFEB        FROM        BVVEPFEB-REG-S
002210     MOVE    WRK-GRAVACAO        TO        WRK-OPERACAO
002220     PERFORM 0700-TESTAR-FS-SELPFEB.
002230
002240*-----------------------------------------------------*
002250 0400-GRAVA-SELPFEB-FIM. EXIT.
002260*-----------------------------------------------------*
002270*---------------------------------------------------------------*
002280 0500-TESTAR-FS SECTION.
002290*---------------------------------------------------------------*
002310     PERFORM 0600-TESTAR-FS-ARQPFEB
002320     PERFORM 0700-TESTAR-FS-SELPFEB.
002330
002340*---------------------------------------------------------------*
002350 0500-TESTAR-FS-FIM. EXIT.
002360*---------------------------------------------------------------*
002370*---------------------------------------------------------------*
002380 0600-TESTAR-FS-ARQPFEB SECTION.
002390*---------------------------------------------------------------*
002400
002410     IF  WRK-FS-ARQPFEB    NOT EQUAL        '00'
002420         DISPLAY '************** BVVE0055 *************'
002430         DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
002440         DISPLAY '*              ARQUIVO              *'
002450         DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQPFEB
002460                                            '         *'
002470         DISPLAY '************** BVVE0055 *************'
002480         CALL 'ILBOABN0'       USING        WRK-ABEND.
002490
002500*---------------------------------------------------------------*
002520*---------------------------------------------------------------*
002530*---------------------------------------------------------------*
002540 0700-TESTAR-FS-SELPFEB SECTION.
002550*---------------------------------------------------------------*
002560
002570     IF  WRK-FS-SELPFEB    NOT EQUAL        '00'
002580         DISPLAY '************** BVVE0055 *************'
002590         DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
002600         DISPLAY '*              SELPFEB              *'
002610         DISPLAY '*         FILE STATUS =  ' WRK-FS-SELPFEB
002620                                            '         *'
002630         DISPLAY '************** BVVE0055 *************'
002640         CALL 'ILBOABN0'       USING        WRK-ABEND.
002650
002660*---------------------------------------------------------------*
002670 0700-TESTAR-FS-SELPFEB-FIM. EXIT.
002680*---------------------------------------------------------------*
