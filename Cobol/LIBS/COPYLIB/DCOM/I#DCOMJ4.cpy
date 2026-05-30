      *----------------------------------------------------------------*
      *    INC = I#DCOMXJ                                              *
      *----------------------------------------------------------------*
      *        AREA DE COMUNICACAO DO MODULO DCOM5537                  *
      *        OBTER DADOS DA OPERACAO TRANSITORIA/OFICIAL/HISTORICO   *
      *                                                                *
      ******************************************************************

       01     5537-ENTRADA-ROTEADOR.
         05   5537-ENT-COD-RETORNO         PIC  X(04).
         05   5537-ENT-MSG-RETORNO         PIC  X(79).
         05   5537-ENT-RESTART             PIC  9(05).
         05   5537-ENT-FLAG                PIC  X(01).
         05   5537-ENT-CFUNC-BDSCO         PIC  9(09).
         05   5537-ENT-CTERM               PIC  X(08).
         05   5537-ENT-DANO-OPER-DESC      PIC  9(04).
         05   5537-ENT-NSEQ-OPER-DESC      PIC  9(09).
         05   5537-ENT-HULT-ATULZ          PIC  X(26).

       01     5537-SAIDA-ROTEADOR.
         05   5537-SAI-COD-RETORNO         PIC X(04).
         05   5537-SAI-MSG-RETORNO         PIC X(79).
         05   5537-SAI-RESTART             PIC 9(05).
         05   5537-SAI-FLAG                PIC X(01).
         05   5537-SAI-DADOS.
           10 5537-SAI-CPRODT              PIC 9(03).
           10 5537-SAI-IPRODT              PIC X(60).
           10 5537-SAI-IABREV-PRODT        PIC X(12).
           10 5537-SAI-CSPROD-DESC-COML    PIC 9(03).
           10 5537-SAI-ISPROD-DESC-COML    PIC X(40).
           10 5537-SAI-IRSUMO-SPROD-DESC  PIC X(10).
           10 5537-SAI-DANO-OPER-DESC      PIC 9(04).
           10 5537-SAI-NSEQ-OPER-DESC      PIC 9(09).
           10 5537-SAI-CCONTR-CONVE-DESC   PIC 9(09).
           10 5537-SAI-CCONTR-LIM-DESC     PIC 9(09).
           10 5537-SAI-CVRSAO-CONTR-CONVE  PIC 9(03).
           10 5537-SAI-NSMULA-OPER-DESC    PIC 9(09).
           10 5537-SAI-CCART-NORML-DESC    PIC X(05).
           10 5537-SAI-DANO-BASE           PIC 9(04).
           10 5537-SAI-CDEPDC              PIC 9(05).
           10 5537-SAI-CSEQ-STUDO          PIC 9(09).
           10 5537-SAI-VOPER-DESC-COML     PIC 9(15)V9(2).
           10 5537-SAI-VTAC-OPER-DESC      PIC 9(15)V9(2).
           10 5537-SAI-PTX-JURO-MES        PIC 9(06)V9(7).
           10 5537-SAI-PTX-JURO-ANO        PIC 9(06)V9(7).
           10 5537-SAI-VTARIF-REG-TITLO    PIC 9(15)V9(2).
           10 5537-SAI-DVCTO-FNAL-OPER     PIC X(10).
           10 5537-SAI-VJURO-OPER-DESC     PIC 9(15)V9(2).
           10 5537-SAI-VIOF-OPER-DESC      PIC 9(15)V9(2).
           10 5537-SAI-QTITLO-CHEQ-PCELA   PIC 9(09).
           10 5537-SAI-VLIQ-OPER-DESC      PIC 9(15)V9(2).
           10 5537-SAI-DINIC-OPER-DESC     PIC X(10).
           10 5537-SAI-VOPER-DESC-APURA    PIC 9(15)V9(2).
           10 5537-SAI-QTITLO-OPER-APURA   PIC 9(09).
           10 5537-SAI-DVCTO-FNAL-APURA    PIC X(10).
           10 5537-SAI-TOPER-DESC-APURA    PIC 9(03).
           10 5537-SAI-TMED-PONDE-OPER     PIC 9(03).
           10 5537-SAI-PMIN-TX-JURO-REGRA  PIC 9(03)V9(03).
           10 5537-SAI-PMAX-TX-JURO-REGRA  PIC 9(03)V9(03).
           10 5537-SAI-CSIT-DESC-COML      PIC 9(03).
           10 5537-SAI-IRSUMO-SIT-DESC     PIC X(15).
           10 5537-SAI-HSIT-DESC-COML      PIC X(26).
           10 5537-SAI-VLIBRC-DESC-COML    PIC 9(15)V9(02).
           10 5537-SAI-VLIBRC-ANTCP-OPER   PIC 9(15)V9(02).
           10 5537-SAI-CBCO                PIC 9(03).
           10 5537-SAI-CAG-BCRIA           PIC 9(05).
           10 5537-SAI-CCTA-BCRIA-CLI      PIC 9(13).
           10 5537-SAI-CBCO-OPER-OPER      PIC 9(03).
           10 5537-SAI-CAG-OPER-OPER       PIC 9(05).
           10 5537-SAI-CTPO-DESC-COML      PIC 9(03).
           10 5537-SAI-ITPO-DESC-COML      PIC X(30).
           10 5537-SAI-IRSUMO-TPO-DESC     PIC X(10).
           10 5537-SAI-CELMTO-DESC-COML    PIC 9(03).
           10 5537-SAI-CINDCD-DIGTC-AG     PIC X(01).
           10 5537-SAI-CINDCD-ISENC-OPER   PIC X(01).
           10 5537-SAI-CINDCD-AUTRZ-OPER   PIC X(01).
           10 5537-SAI-CINDCD-MODLD-INCL   PIC X(01).
           10 5537-SAI-CCTRO-CUSTO         PIC X(04).
           10 5537-SAI-CINDCD-APROV-CREDT  PIC X(01).
           10 5537-SAI-CMEIO-ENTRD-DESC    PIC 9(03).
           10 5537-SAI-IRSUMO-MEIO-ENTRD   PIC X(15).
           10 5537-SAI-CGARNT              PIC 9(03).
           10 5537-SAI-ITPO-GARNT          PIC X(40).
           10 5537-SAI-QPCELA-OPER         PIC 9(05).
           10 5537-SAI-NAUTRZ-TRNSM-ESCR   PIC 9(07).
           10 5537-SAI-CINDCD-PGDOR-JURO   PIC X(01).
           10 5537-SAI-CTPO-TX-JURO-DESC   PIC 9(03).
           10 5537-SAI-ITPO-TX-JURO-DESC   PIC X(30).
           10 5537-SAI-IRSUMO-TPO-TX-JURO  PIC X(15).
           10 5537-SAI-CSGL-TPO-TX-JURO    PIC X(06).
           10 5537-SAI-PALIQT-IOF-DESC     PIC 9(03)V9(05).
           10 5537-SAI-PTX-CUSTO-MES       PIC 9(03)V9(06).
           10 5537-SAI-PTX-CUSTO-ANO       PIC 9(03)V9(06).
           10 5537-SAI-VJURO-CUSTO-OPER    PIC 9(15)V9(02).
           10 5537-SAI-VPSENT-OPER-DESC    PIC 9(15)V9(02).
           10 5537-SAI-CINDCD-VALDC-ROTRO  PIC X(01).
           10 5537-SAI-DVALDC-ROTRO-ELMTO  PIC X(10).
           10 5537-SAI-CFUNC-VALDC-ROTRO   PIC 9(09).
           10 5537-SAI-DULT-ADTTO-OPER     PIC X(10).
           10 5537-SAI-CINDCD-MOTVO-ADTTO  PIC X(01).
           10 5537-SAI-HULT-ATULZ          PIC X(26).
           10 5537-SAI-CFUNC-BDSCO         PIC 9(09).
           10 5537-SAI-CTERM               PIC X(08).
           10 5537-SAI-CCONTR-CORP-OPER    PIC 9(07).
           10 5537-SAI-DINCL-REG           PIC X(10).
           10 5537-SAI-CCART-LIM-CREDT     PIC X(05).
           10 5537-SAI-ESPECIE-TITLO       PIC 9(03).
           10 5537-SAI-IPSSOA-DESC-COML    PIC X(60).
           10 5537-SAI-CCNPJ-CPF           PIC 9(09).
           10 5537-SAI-CFLIAL-CNPJ         PIC 9(04).
           10 5537-SAI-CCTRL-CNPJ-CPF      PIC 9(02).
           10 5537-SAI-CDOCTO-ID           PIC X(15).
           10 5537-SAI-NOME-BANCO          PIC X(40).
           10 5537-SAI-NOME-AGENCIA        PIC X(40).
           10 5537-SAI-CCHAVE-ELEMENTO     PIC X(45).
           10 5537-SAI-CINDCD-ALIQT-REDZD  PIC X(01).
           10 5537-SAI-PTX-JURO-DIA        PIC 9(03)V9(05).
           10 5537-SAI-CINDCD-FREQ-TX      PIC X(02).
           10 5537-SAI-CET-AM              PIC 9(05)V99.
           10 5537-SAI-CET-AA              PIC 9(05)V99.
           10 5537-RESERVA                 PIC X(36).

         05   5537-SAI-CONSISTENCIA.
           10 5537-SAI-TAMANHO-ERRO        PIC  9(01).
           10 5537-SAI-TABELA-ERROS OCCURS 2 TIMES.
              15 5537-SAI-CAMPOS-ERRO      PIC  9(01).

