      *----------------------------------------------------------------*
      * BOOK I#BVVECA     CADASTRO CELO (VENDEDOR E PORTADOR)          *
      * TAMANHO : 0350    COMPRIMIDO                                   *
      * DSN AF.CELO.PRV.CADASTRO(0)  CB.BACC.PRV.CADACELO(0)           *
      * CHAVE CLAS.: BANCO, TIPO, BCO, NUM1, NUM2.                     *
      * CONTEM REG. VENDEDOR (TIPO 1) E PORTADOR (TIPO 2)              *
      *----------------------------------------------------------------*
      *                                                   POS.INIC  TAM.
       01  CAD-REG.
      *                                                        001  350
           02 CAD-CARTAO.
      *                                                        001  011
              03 CAD-BCO               PIC 9(05) COMP-3.
      *          4560 = VENDEDOR                               001  003
      *          0000 = PORTADOR CARTAO 423700
      *          0001 = GOLD   BIN 453211 INST. 001005
      *          0002 = LOCAL  BIN 492052 INST. 001003
      *          0007 = BUSIN. BIN 453207 INST. 001001
      *          0008 = WORLD  BIN 456508 INST. 001002
      *
              03 CAD-NUM1              PIC 9(05) COMP-3.
      *                                                        004  003
              03 CAD-TIPO              PIC 9(01).
      *          1 = VENDEDOR                                  007  001
      *          2 = PORTADOR
      *
              03 CAD-NUM2              PIC 9(07) COMP-3.
      *                                                        008  004
           02 CAD-BANCO                PIC 9(03) COMP-3.
      *       00 = VENDEDOR OU ANTIGO CARTAO 423700            012  002
      *       NN = PORTADOR
      *       31 = REG. TOTAL
      *
           02 CAD-AGENC                PIC 9(05) COMP-3.
      *                                                        014  003
           02 CAD-GRUPO                PIC 9(05) COMP-3.
      *                                                        017  003
           02 CAD-CONTA                PIC 9(07) COMP-3.
      *                                                        020  004
      *       0 = VENDEDOR OU TITULAR                          024  001
      *       1 = DEPENDENTE
      *
           02 CAD-NOME.
      *                                                        025  035
              03 CAD-ASTER             PIC X(02).
      *                                                        025  002
              03 FILLER                PIC X(24).
      *                                                        027  024
              03 CAD-TOTREG            PIC X(09).
      *                                                        051  009
           02 CAD-ENDERECO.
      *                                                        060  035
              03 CAD-ENDASTER          PIC X(02).
      *          ** = VENDEDOR CANCELADO                       060  002
      *
              03 FILLER                PIC X(33).
      *                                                        062  033
           02 CAD-CEP                  PIC 9(05).
      *                                                        095  005
      *       D A D O S   D O   V E N D E D O R
      *
              03 CAD-DTATUALIZ1        PIC 9(07)    COMP-3.
      *          FORMATO DDMMAA                                100  004
      *
              03 CAD-DTMOVTO1          PIC 9(07)    COMP-3.
      *          FORMATO DDMMAA                                104  004
      *
              03 CAD-VD-VISTATU-ORP    PIC 9(13)V99 COMP-3.
      *                                                        108  008
              03 CAD-COM-VISTATU-ORP   PIC 9(11)V99 COMP-3.
      *                                                        116  007
              03 CAD-VD-VISTATU-ADS    PIC 9(13)V99 COMP-3.
      *                                                        123  008
              03 CAD-COM-VISTATU-ADS   PIC 9(11)V99 COMP-3.
      *                                                        131  007
              03 CAD-VD-VISTPOS-ORP    PIC 9(13)V99 COMP-3.
      *                                                        138  008
              03 CAD-COM-VISTPOS-ORP   PIC 9(11)V99 COMP-3.
      *                                                        146  007
      *                                                        153  008
              03 CAD-COM-VISTPOS-ADS   PIC 9(11)V99 COMP-3.
      *                                                        161  007
              03 CAD-VD-PRAZATU-ORP    PIC 9(13)V99 COMP-3.
      *                                                        168  008
              03 CAD-COM-PRAZATU-ORP   PIC 9(11)V99 COMP-3.
      *                                                        176  007
              03 CAD-VD-PRAZATU-ADS    PIC 9(13)V99 COMP-3.
      *                                                        183  008
              03 CAD-COM-PRAZATU-ADS   PIC 9(11)V99 COMP-3.
      *                                                        191  007
              03 CAD-VD-PRAZPOS-ORP    PIC 9(13)V99 COMP-3.
      *                                                        198  008
              03 CAD-COM-PRAZPOS-ORP   PIC 9(11)V99 COMP-3.
      *                                                        206  007
              03 CAD-VD-PRAZPOS-ADS    PIC 9(13)V99 COMP-3.
      *                                                        213  008
              03 CAD-COM-PRAZPOS-ADS   PIC 9(11)V99 COMP-3.
      *                                                        221  007
              03 CAD-VD-TOTVIST-ANT    PIC 9(13)V99 COMP-3.
              03 CAD-VD-TOTVIST-ANO    PIC 9(13)V99 COMP-3.
      *                                                        236  008
              03 CAD-VD-TOTPRAZ-ANT    PIC 9(13)V99 COMP-3.
      *                                                        244  008
              03 CAD-VD-TOTPRAZ-ANO    PIC 9(13)V99 COMP-3.
      *                                                        252  008
              03 CAD-TAXAS.
                 04 CAD-ADS-VIST       PIC 9(05) COMP-3.
      *                                                        260  003
                 04 CAD-ORP-VIST       PIC 9(05) COMP-3.
      *                                                        263  003
                 04 CAD-ADS-PRAZ       PIC 9(05) COMP-3.
      *                                                        266  003
                 04 CAD-ORP-PRAZ       PIC 9(05) COMP-3.
      *                                                        269  003
              03 CAD-CREDITO           PIC X(01).
      *          N = NORMAL R$                                 272  001
      *          R = ROTATIVO R$
      *          F = FIXO R$
      *          V = ROTATIVO VISA R$
      *          Z = ROTATIVO U$ PARC. VENDEDOR
      *          W = ROTATIVO R$ PARC. VENDEDOR
      *          Y = FIXO U$ PARC. VENDEDOR
      *          K = FIXO R$ PARC. VENDEDOR
      *
              03 CAD-DIAS              PIC 9(02).
      *          QTD. DIAS DE CREDITO PARA CRED. R V Z W       273  002
      *          DIA DO CREDITO PARA CRED. F U Y K
      *
              03 CAD-LIMOPER           PIC 9(07)V99 COMP-3.
      *                                                        275  005
              03 CAD-RAMO              PIC 9(02).
      *                                                        280  002
              03 CAD-CGCCPF1           PIC 9(13) COMP-3.
      *          FORMATO NNNNNNNNFFFDD  PARA CGC               282  007
      *          FORMATO NNNNNNNNNNNDD  PARA CPF
      *
              03 CAD-FONE1             PIC 9(07) COMP-3.
      *                                                        289  004
              03 CAD-TXMAQ             PIC X(01).
      *          SPACES = COBRA TAXA
      *
              03 CAD-HOTCARD           PIC X(01).
      *          1 = RECEBE HOTCARD                            294  001
      *          2 = NAO RECEBE HOTCARD
      *
              03 CAD-SUFCEP            PIC X(03).
      *                                                        295  003
              03 CAD-FILIAIS           PIC 9(03) COMP-3.
      *                                                        298  002
              03 CAD-DTADESAO          PIC 9(07) COMP-3.
      *          FORMATO DDMMAA                                300  004
      *
              03 CAD-QTDMAQ            PIC 9(05) COMP-3.
      *                                                        304  003
              03 CAD-CODATIV           PIC 9(05) COMP-3.
      *                                                        307  003
              03 CAD-RAZCOMERC.
      *                                                        310  040
                 04 FILLER             PIC X(06).
                 04 FILLER             PIC X(33).
              03 CAD-KELO1             PIC 9(01).
      *                                                        350  001
           02 CAD-TIPO2  REDEFINES CAD-TIPO1.
      *       D A D O S   D O   P O R T A D O R
      *
              03 CAD-CGCCPF2           PIC 9(13) COMP-3.
      *                                                        100  007
              03 CAD-FONE2             PIC 9(07) COMP-3.
      *                                                        107  004
              03 CAD-DIADEBIT          PIC 9(02).
      *                                                        111  002
              03 CAD-RENOVADO          PIC X(01).
      *          SPACES = ATIVO                                113  001
      *          V = VENCIDO
      *          T = TROCADO
      *          S = SUBSTITUIDO
      *          A = CARTAO VISA - ACI
      *          R = RENOVADO
      *
      *          FORMATO AAMMDD                                114  004
      *
              03 CAD-ENDE              PIC 9(01).
      *          0 = AGENCIA/CHEQ                              118  001
      *          1 = CORREIO/CHEQ
      *          2 = AGENCIA/DIGITADO
      *          3 = CORREIO/DIGITADO
      *
              03 CAD-SEXO              PIC X(01).
      *                                                        119  001
              03 CAD-DTNASC            PIC 9(07) COMP-3.
      *          FORMATO DDMMAA                                120  004
      *
              03 CAD-PIN               PIC 9(07) COMP-3.
      *                                                        124  004
              03 CAD-PVV               PIC 9(04).
      *                                                        128  004
              03 CAD-TPCT              PIC 9(03) COMP-3.
      *          00 = TITULAR MULT                             132  002
      *          11 = TITULAR MULT
      *          13 = DEPEND. MULT
      *          14 = CARTAO DE CREDITO
      *
              03 CAD-MARCA             PIC 9.
      *          1 = SENHA DO CLIENTE/MULT                     134  001
      *          2 = SENHA AUTOMATICA/MULT
      *          3 = SENHA DO CLIENTE/CCB
      *          4 = SENHA AUTOMATICA/CCB
      *          4 = SENHA VIA ACI (AUTOMAT.)
      *
              03 CAD-BANCAV            PIC X(01).
      *          0 = NAO BANCA A VISTA                         135  001
      *          1 = BANCA A VISTA
      *
              03 CAD-BANCAP            PIC X(01).
      *          0 = NAO BANCA A PRAZO                         136  001
      *          1 = BANCA A PRAZO
      *
              03 CAD-FIANCA            PIC X(01).
      *          0 = NAO FUNCIONARIO - COBRA TAXA              137  001
      *          2 = NAO FUNCIONARIO - NAO COBRA INICIAL
      *          3 = FUNCIONARIO     - NAO COBRA INICIAL
      *          4 = NAO FUNCIONARIO - NAO COBRA PERMANENTE
      *          5 = FUNCIONARIO     - NAO COBRA PERMANENTE
      *
              03 CAD-EXTRAV            PIC X(01).
      *          SPACES = ATIVO                                138  001
      *          * = FURTO
      *          ) = EXTRAVIO
      *          X = ORDEM ADMINISTRATIVA
      *          / = DESINTERESSE
      *          $ = BLOQUEADO
      *          1 = NAO RENOV.P/SOLIC.AGENCIA
      *
              03 CAD-SDATU-VIST        PIC 9(09)V99 COMP-3.
      *                                                        139  006
              03 CAD-VLDEBTDO-AV       PIC 9(09)V99 COMP-3.
      *                                                        145  006
              03 CAD-VLADEBIT-AV       PIC 9(09)V99 COMP-3.
      *                                                        151  006
      *                                                        157  005
              03 CAD-VLDEB5PC-AV       PIC 9(07)V99 COMP-3.
      *                                                        162  005
              03 CAD-SDPOST-VIST       PIC 9(09)V99 COMP-3.
      *                                                        167  006
              03 CAD-VLDEBTDO-PV       PIC 9(09)V99 COMP-3.
      *                                                        173  006
              03 CAD-VLADEBIT-PV       PIC 9(09)V99 COMP-3.
      *                                                        179  006
              03 CAD-VLCNCC30-PV       PIC 9(07)V99 COMP-3.
      *                                                        185  005
              03 CAD-VLDEB5PC-PV       PIC 9(07)V99 COMP-3.
      *                                                        190  005
              03 CAD-SDATU-PRAZ        PIC 9(09)V99 COMP-3.
      *                                                        195  006
              03 CAD-VLDEBTDO-AP       PIC 9(09)V99 COMP-3.
      *                                                        201  006
              03 CAD-VLADEBIT-AP       PIC 9(09)V99 COMP-3.
      *                                                        207  006
              03 CAD-VLCNCC30-AP       PIC 9(07)V99 COMP-3.
              03 CAD-VLDEB5PC-AP       PIC 9(07)V99 COMP-3.
      *                                                        218  005
              03 CAD-SDPOST-PRAZ       PIC 9(09)V99 COMP-3.
      *                                                        223  006
              03 CAD-VLDEBTDO-PP       PIC 9(09)V99 COMP-3.
      *                                                        229  006
              03 CAD-VLADEBIT-PP       PIC 9(09)V99 COMP-3.
      *                                                        235  006
              03 CAD-VLCNCC30-PP       PIC 9(07)V99 COMP-3.
      *                                                        241  005
              03 CAD-VLDEB5PC-PP       PIC 9(07)V99 COMP-3.
      *                                                        246  005
              03 CAD-TOTCP-ANO         PIC 9(09)V99 COMP-3.
      *                                                        251  006
              03 CAD-TOTCP-MESANT      PIC 9(09)V99 COMP-3.
      *                                                        257  006
              03 CAD-TOTCP-MESATU      PIC 9(09)V99 COMP-3.
      *                                                        263  006
              03 CAD-DTDEBANT          PIC 9(07) COMP-3.
      *          FORMATO DDMMAA                                269  004
              03 CAD-DTULTMOV2         PIC 9(07) COMP-3.
      *          FORMATO DDMMAA                                273  004
      *
              03 CAD-DTCANC            PIC 9(07) COMP-3.
      *          FORMATO DDMMAA                                277  004
      *
              03 CAD-DTATUALIZ2        PIC 9(07) COMP-3.
      *          FORMATO DDMMAA                                281  004
      *
              03 CAD-VCTOCARTAO        PIC 9(07) COMP-3.
      *          FORMATO 000AAMM                               285  004
      *
              03 CAD-LIMITE            PIC 9(07) COMP-3.
      *                                                        289  004
              03 CAD-PROPOSTA          PIC 9(07) COMP-3.
      *                                                        293  004
              03 CAD-DTCADAS           PIC 9(05) COMP-3.
      *          FORMATO AADDD                                 297  003
      *
              03 CAD-EXCESSO           PIC 9(01).
      *          9 = TETO MENOR
      *
              03 CAD-MESANT2           PIC X(04).
      *                                                        301  004
              03 CAD-MESANT1           PIC X(04).
      *                                                        305  004
              03 CAD-EXCSALDO          PIC 9(07) COMP-3.
      *                                                        309  004
              03 CAD-DTEMIS            PIC 9(05) COMP-3.
      *          FORMATO AADDD                                 313  003
      *
              03 CAD-FJ2               PIC 9(01).
      *          0 = FISICA                                    316  001
      *          1 = JURIDICA
      *
              03 CAD-VLDEBATU          PIC 9(09)V99 COMP-3.
      *                                                        317  006
              03 CAD-VLDEBPOS          PIC 9(09)V99 COMP-3.
      *                                                        323  006
              03 CAD-TXEXPATU          PIC 9(07)V99 COMP-3.
              03 CAD-TXEXPPOS          PIC 9(07)V99 COMP-3.
      *                                                        334  005
              03 CAD-COMISATU          PIC 9(07)V99 COMP-3.
      *                                                        339  005
              03 CAD-COMISPOS          PIC 9(07)V99 COMP-3.
      *                                                        344  005
              03 CAD-PENDENCIA         PIC X(01).
      *          * = PENDENTE DE ENTREGA                       349  001
      *          SPACES = ENTREGUE
      *
              03 CAD-KELO2             PIC 9(01).
      *          1 = INCLUSAO                                  350  001
      *          2 = ALTER. DB NOME (NOME, ENDER.,CEP)
      *          3 = ALTER. DB CARTAO
      *          4 = NOME E CARTAO
