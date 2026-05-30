      *----------------------------------------------------------------*
      *    INC = I#DCOMXJ                                              *
      *----------------------------------------------------------------*
      *        AREA DE COMUNICACAO DO MODULO DCOM4174                  *
      *        OBTER DADOS DA OPERACAO TRANSITORIA/OFICIAL/HISTORICO   *
      *                                                                *
      ******************************************************************

       01     4174-ENTRADA-ROTEADOR.
         05   4174-ENT-COD-RETORNO         PIC  X(04).
         05   4174-ENT-MSG-RETORNO         PIC  X(79).
         05   4174-ENT-RESTART             PIC  9(05).
         05   4174-ENT-FLAG                PIC  X(01).
         05   4174-ENT-CFUNC-BDSCO         PIC  9(09).
         05   4174-ENT-CTERM               PIC  X(08).
         05   4174-ENT-DANO-OPER-DESC      PIC  9(04).
         05   4174-ENT-NSEQ-OPER-DESC      PIC  9(09).

       01     4174-SAIDA-ROTEADOR.
         05   4174-SAI-COD-RETORNO         PIC X(04).
         05   4174-SAI-MSG-RETORNO         PIC X(79).
         05   4174-SAI-RESTART             PIC 9(05).
         05   4174-SAI-FLAG                PIC X(01).
         05   4174-SAI-DADOS.
           10 4174-SAI-CPRODT              PIC 9(03).
           10 4174-SAI-IPRODT              PIC X(60).
           10 4174-SAI-IABREV-PRODT        PIC X(12).
           10 4174-SAI-CSPROD-DESC-COML    PIC 9(03).
           10 4174-SAI-ISPROD-DESC-COML    PIC X(40).
           10 4174-SAI-IRSUMO-SPROD-DESC  PIC X(10).
           10 4174-SAI-DANO-OPER-DESC      PIC 9(04).
           10 4174-SAI-NSEQ-OPER-DESC      PIC 9(09).
           10 4174-SAI-CCONTR-CONVE-DESC   PIC 9(09).
           10 4174-SAI-CCONTR-LIM-DESC     PIC 9(09).
           10 4174-SAI-CVRSAO-CONTR-CONVE  PIC 9(03).
           10 4174-SAI-NSMULA-OPER-DESC    PIC 9(09).
           10 4174-SAI-CCART-NORML-DESC    PIC X(05).
           10 4174-SAI-DANO-BASE           PIC 9(04).
           10 4174-SAI-CDEPDC              PIC 9(05).
           10 4174-SAI-CSEQ-STUDO          PIC 9(09).
           10 4174-SAI-VOPER-DESC-COML     PIC 9(15)V9(2).
           10 4174-SAI-VTAC-OPER-DESC      PIC 9(15)V9(2).
           10 4174-SAI-PTX-JURO-MES        PIC 9(06)V9(7).
           10 4174-SAI-PTX-JURO-ANO        PIC 9(06)V9(7).
           10 4174-SAI-VTARIF-REG-TITLO    PIC 9(15)V9(2).
           10 4174-SAI-DVCTO-FNAL-OPER     PIC X(10).
           10 4174-SAI-VJURO-OPER-DESC     PIC 9(15)V9(2).
           10 4174-SAI-VIOF-OPER-DESC      PIC 9(15)V9(2).
           10 4174-SAI-QTITLO-CHEQ-PCELA   PIC 9(09).
           10 4174-SAI-VLIQ-OPER-DESC      PIC 9(15)V9(2).
           10 4174-SAI-DINIC-OPER-DESC     PIC X(10).
           10 4174-SAI-VOPER-DESC-APURA    PIC 9(15)V9(2).
           10 4174-SAI-QTITLO-OPER-APURA   PIC 9(09).
           10 4174-SAI-DVCTO-FNAL-APURA    PIC X(10).
           10 4174-SAI-TOPER-DESC-APURA    PIC 9(03).
           10 4174-SAI-TMED-PONDE-OPER     PIC 9(03).
           10 4174-SAI-PMIN-TX-JURO-REGRA  PIC 9(03)V9(03).
           10 4174-SAI-PMAX-TX-JURO-REGRA  PIC 9(03)V9(03).
           10 4174-SAI-CSIT-DESC-COML      PIC 9(03).
           10 4174-SAI-IRSUMO-SIT-DESC     PIC X(15).
           10 4174-SAI-HSIT-DESC-COML      PIC X(26).
           10 4174-SAI-VLIBRC-DESC-COML    PIC 9(15)V9(02).
           10 4174-SAI-VLIBRC-ANTCP-OPER   PIC 9(15)V9(02).
           10 4174-SAI-CBCO                PIC 9(03).
           10 4174-SAI-CAG-BCRIA           PIC 9(05).
           10 4174-SAI-CCTA-BCRIA-CLI      PIC 9(13).
           10 4174-SAI-CBCO-OPER-OPER      PIC 9(03).
           10 4174-SAI-CAG-OPER-OPER       PIC 9(05).
           10 4174-SAI-CTPO-DESC-COML      PIC 9(03).
           10 4174-SAI-ITPO-DESC-COML      PIC X(30).
           10 4174-SAI-IRSUMO-TPO-DESC     PIC X(10).
           10 4174-SAI-CELMTO-DESC-COML    PIC 9(03).
           10 4174-SAI-CINDCD-DIGTC-AG     PIC X(01).
           10 4174-SAI-CINDCD-ISENC-OPER   PIC X(01).
           10 4174-SAI-CINDCD-AUTRZ-OPER   PIC X(01).
           10 4174-SAI-CINDCD-MODLD-INCL   PIC X(01).
           10 4174-SAI-CCTRO-CUSTO         PIC X(04).
           10 4174-SAI-CINDCD-APROV-CREDT  PIC X(01).
           10 4174-SAI-CMEIO-ENTRD-DESC    PIC 9(03).
           10 4174-SAI-IRSUMO-MEIO-ENTRD   PIC X(15).
           10 4174-SAI-CGARNT              PIC 9(03).
           10 4174-SAI-ITPO-GARNT          PIC X(40).
           10 4174-SAI-QPCELA-OPER         PIC 9(05).
           10 4174-SAI-NAUTRZ-TRNSM-ESCR   PIC 9(07).
           10 4174-SAI-CINDCD-PGDOR-JURO   PIC X(01).
           10 4174-SAI-CTPO-TX-JURO-DESC   PIC 9(03).
           10 4174-SAI-ITPO-TX-JURO-DESC   PIC X(30).
           10 4174-SAI-IRSUMO-TPO-TX-JURO  PIC X(15).
           10 4174-SAI-CSGL-TPO-TX-JURO    PIC X(06).
           10 4174-SAI-PALIQT-IOF-DESC     PIC 9(03)V9(05).
           10 4174-SAI-PTX-CUSTO-MES       PIC 9(03)V9(06).
           10 4174-SAI-PTX-CUSTO-ANO       PIC 9(03)V9(06).
           10 4174-SAI-VJURO-CUSTO-OPER    PIC 9(15)V9(02).
           10 4174-SAI-VPSENT-OPER-DESC    PIC 9(15)V9(02).
           10 4174-SAI-CINDCD-VALDC-ROTRO  PIC X(01).
           10 4174-SAI-DVALDC-ROTRO-ELMTO  PIC X(10).
           10 4174-SAI-CFUNC-VALDC-ROTRO   PIC 9(09).
           10 4174-SAI-DULT-ADTTO-OPER     PIC X(10).
           10 4174-SAI-CINDCD-MOTVO-ADTTO  PIC X(01).
           10 4174-SAI-HULT-ATULZ          PIC X(26).
           10 4174-SAI-CFUNC-BDSCO         PIC 9(09).
           10 4174-SAI-CTERM               PIC X(08).
           10 4174-SAI-CCONTR-CORP-OPER    PIC 9(07).
           10 4174-SAI-DINCL-REG           PIC X(10).
           10 4174-SAI-CCART-LIM-CREDT     PIC X(05).
           10 4174-SAI-ESPECIE-TITLO       PIC 9(03).
           10 4174-SAI-IPSSOA-DESC-COML    PIC X(60).
4S2511*    10 4174-SAI-CCNPJ-CPF           PIC 9(09).
4S2511     10 4174-SAI-CCNPJ-CPF           PIC X(09).
4S2511*    10 4174-SAI-CFLIAL-CNPJ         PIC 9(04).
4S2511     10 4174-SAI-CFLIAL-CNPJ         PIC X(04).
           10 4174-SAI-CCTRL-CNPJ-CPF      PIC 9(02).
           10 4174-SAI-CDOCTO-ID           PIC X(15).
           10 4174-SAI-NOME-BANCO          PIC X(40).
           10 4174-SAI-NOME-AGENCIA        PIC X(40).
           10 4174-SAI-CCHAVE-ELEMENTO     PIC X(45).
           10 4174-SAI-CINDCD-ALIQT-REDZD  PIC X(01).
           10 4174-SAI-PTX-JURO-DIA        PIC 9(03)V9(05).
           10 4174-SAI-CINDCD-FREQ-TX      PIC X(02).
           10 4174-SAI-CET-AM              PIC 9(05)V99.
           10 4174-SAI-CET-AA              PIC 9(05)V99.
BSI001     10 4174-SAI-QTDE-BANDEIRAS      PIC 9(09).
BSI001     10 4174-RESERVA                 PIC X(27).

         05   4174-SAI-CONSISTENCIA.
           10 4174-SAI-TAMANHO-ERRO        PIC  9(01).
           10 4174-SAI-TABELA-ERROS OCCURS 2 TIMES.
              15 4174-SAI-CAMPOS-ERRO      PIC  9(01).