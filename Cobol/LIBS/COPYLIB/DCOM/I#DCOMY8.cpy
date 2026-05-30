      *----------------------------------------------------------------*
      *    INC = I#DCOMY8                                              *
      *----------------------------------------------------------------*
      *        AREA DE COMUNICACAO DO MODULO DCOM5542                  *
      *        OBTER DADOS DA OPERACAO TRANSITORIA/OFICIAL/HISTORICO   *
      *        POR NUMERO CORPORATIVO.                                 *
      ******************************************************************
      *                        ALTERACAO BSI                           *
      *----------------------------------------------------------------*
      *    DATA....: 31/05/2013                                        *
      *    OBJETIVO: INCLUIR CAMPO 5542-ENT-TIPO-PESQUISA A FIM DE VE- *
      *              RIFICAR SE O TIPO DE  PESQUISA E  POR CEDENTE OU  *
      *              EMITENTE DA OPERACAO.                             *
      *================================================================*

       01     5542-ENTRADA-ROTEADOR.
         05   5542-ENT-COD-RETORNO         PIC  X(04).
         05   5542-ENT-MSG-RETORNO         PIC  X(79).
         05   5542-ENT-RESTART             PIC  9(05).
         05   5542-ENT-FLAG                PIC  X(01).
         05   5542-ENT-CFUNC-BDSCO         PIC  9(09).
         05   5542-ENT-CTERM               PIC  X(08).
BSI001   05   5542-ENT-COD-DEPTO           PIC  9(05).
         05   5542-ENT-NRO-CORPORATIVO     PIC  9(07).
         05   5542-ENT-AGENCIA             PIC  9(05).
         05   5542-ENT-CONTA               PIC  9(13).
         05   5542-ENT-OPCAO               PIC  X(01).
BSI001   05   5542-ENT-TIPO-PESQUISA       PIC  X(01).

       01     5542-SAIDA-ROTEADOR.
         05   5542-SAI-COD-RETORNO         PIC X(04).
         05   5542-SAI-MSG-RETORNO         PIC X(79).
         05   5542-SAI-RESTART             PIC 9(05).
         05   5542-SAI-FLAG                PIC X(01).
         05   5542-SAI-DADOS.
           10 5542-SAI-CPRODT              PIC 9(03).
           10 5542-SAI-IPRODT              PIC X(60).
           10 5542-SAI-IABREV-PRODT        PIC X(12).
           10 5542-SAI-CSPROD-DESC-COML    PIC 9(03).
           10 5542-SAI-ISPROD-DESC-COML    PIC X(40).
           10 5542-SAI-IRSUMO-SPROD-DESC  PIC X(10).
           10 5542-SAI-DANO-OPER-DESC      PIC 9(04).
           10 5542-SAI-NSEQ-OPER-DESC      PIC 9(09).
           10 5542-SAI-CCONTR-CONVE-DESC   PIC 9(09).
           10 5542-SAI-CCONTR-LIM-DESC     PIC 9(09).
           10 5542-SAI-CVRSAO-CONTR-CONVE  PIC 9(03).
           10 5542-SAI-NSMULA-OPER-DESC    PIC 9(09).
           10 5542-SAI-CCART-NORML-DESC    PIC X(05).
           10 5542-SAI-DANO-BASE           PIC 9(04).
           10 5542-SAI-CDEPDC              PIC 9(05).
           10 5542-SAI-CSEQ-STUDO          PIC 9(09).
           10 5542-SAI-VOPER-DESC-COML     PIC 9(15)V9(2).
           10 5542-SAI-VTAC-OPER-DESC      PIC 9(15)V9(2).
           10 5542-SAI-PTX-JURO-MES        PIC 9(06)V9(7).
           10 5542-SAI-PTX-JURO-ANO        PIC 9(06)V9(7).
           10 5542-SAI-VTARIF-REG-TITLO    PIC 9(15)V9(2).
           10 5542-SAI-DVCTO-FNAL-OPER     PIC X(10).
           10 5542-SAI-VJURO-OPER-DESC     PIC 9(15)V9(2).
           10 5542-SAI-VIOF-OPER-DESC      PIC 9(15)V9(2).
           10 5542-SAI-QTITLO-CHEQ-PCELA   PIC 9(09).
           10 5542-SAI-VLIQ-OPER-DESC      PIC 9(15)V9(2).
           10 5542-SAI-DINIC-OPER-DESC     PIC X(10).
           10 5542-SAI-VOPER-DESC-APURA    PIC 9(15)V9(2).
           10 5542-SAI-QTITLO-OPER-APURA   PIC 9(09).
           10 5542-SAI-DVCTO-FNAL-APURA    PIC X(10).
           10 5542-SAI-TOPER-DESC-APURA    PIC 9(03).
           10 5542-SAI-TMED-PONDE-OPER     PIC 9(03).
           10 5542-SAI-PMIN-TX-JURO-REGRA  PIC 9(03)V9(03).
           10 5542-SAI-PMAX-TX-JURO-REGRA  PIC 9(03)V9(03).
           10 5542-SAI-CSIT-DESC-COML      PIC 9(03).
           10 5542-SAI-IRSUMO-SIT-DESC     PIC X(15).
           10 5542-SAI-HSIT-DESC-COML      PIC X(26).
           10 5542-SAI-VLIBRC-DESC-COML    PIC 9(15)V9(02).
           10 5542-SAI-VLIBRC-ANTCP-OPER   PIC 9(15)V9(02).
           10 5542-SAI-CBCO                PIC 9(03).
           10 5542-SAI-CAG-BCRIA           PIC 9(05).
           10 5542-SAI-CCTA-BCRIA-CLI      PIC 9(13).
           10 5542-SAI-CBCO-OPER-OPER      PIC 9(03).
           10 5542-SAI-CAG-OPER-OPER       PIC 9(05).
           10 5542-SAI-CTPO-DESC-COML      PIC 9(03).
           10 5542-SAI-ITPO-DESC-COML      PIC X(30).
           10 5542-SAI-IRSUMO-TPO-DESC     PIC X(10).
           10 5542-SAI-CELMTO-DESC-COML    PIC 9(03).
           10 5542-SAI-CINDCD-DIGTC-AG     PIC X(01).
           10 5542-SAI-CINDCD-ISENC-OPER   PIC X(01).
           10 5542-SAI-CINDCD-AUTRZ-OPER   PIC X(01).
           10 5542-SAI-CINDCD-MODLD-INCL   PIC X(01).
           10 5542-SAI-CCTRO-CUSTO         PIC X(04).
           10 5542-SAI-CINDCD-APROV-CREDT  PIC X(01).
           10 5542-SAI-CMEIO-ENTRD-DESC    PIC 9(03).
           10 5542-SAI-IRSUMO-MEIO-ENTRD   PIC X(15).
           10 5542-SAI-CGARNT              PIC 9(03).
           10 5542-SAI-ITPO-GARNT          PIC X(40).
           10 5542-SAI-QPCELA-OPER         PIC 9(05).
           10 5542-SAI-NAUTRZ-TRNSM-ESCR   PIC 9(07).
           10 5542-SAI-CINDCD-PGDOR-JURO   PIC X(01).
           10 5542-SAI-CTPO-TX-JURO-DESC   PIC 9(03).
           10 5542-SAI-ITPO-TX-JURO-DESC   PIC X(30).
           10 5542-SAI-IRSUMO-TPO-TX-JURO  PIC X(15).
           10 5542-SAI-CSGL-TPO-TX-JURO    PIC X(06).
           10 5542-SAI-PALIQT-IOF-DESC     PIC 9(03)V9(05).
           10 5542-SAI-PTX-CUSTO-MES       PIC 9(03)V9(06).
           10 5542-SAI-PTX-CUSTO-ANO       PIC 9(03)V9(06).
           10 5542-SAI-VJURO-CUSTO-OPER    PIC 9(15)V9(02).
           10 5542-SAI-VPSENT-OPER-DESC    PIC 9(15)V9(02).
           10 5542-SAI-CINDCD-VALDC-ROTRO  PIC X(01).
           10 5542-SAI-DVALDC-ROTRO-ELMTO  PIC X(10).
           10 5542-SAI-CFUNC-VALDC-ROTRO   PIC 9(09).
           10 5542-SAI-DULT-ADTTO-OPER     PIC X(10).
           10 5542-SAI-CINDCD-MOTVO-ADTTO  PIC X(01).
           10 5542-SAI-HULT-ATULZ          PIC X(26).
           10 5542-SAI-CFUNC-BDSCO         PIC 9(09).
           10 5542-SAI-CTERM               PIC X(08).
           10 5542-SAI-CCONTR-CORP-OPER    PIC 9(07).
           10 5542-SAI-DINCL-REG           PIC X(10).
           10 5542-SAI-CCART-LIM-CREDT     PIC X(05).
           10 5542-SAI-ESPECIE-TITLO       PIC 9(03).
           10 5542-SAI-IPSSOA-DESC-COML    PIC X(60).
           10 5542-SAI-CCNPJ-CPF           PIC 9(09).
           10 5542-SAI-CFLIAL-CNPJ         PIC 9(04).
           10 5542-SAI-CCTRL-CNPJ-CPF      PIC 9(02).
           10 5542-SAI-CDOCTO-ID           PIC X(15).
           10 5542-SAI-NOME-BANCO          PIC X(40).
           10 5542-SAI-NOME-AGENCIA        PIC X(40).
           10 5542-SAI-CCHAVE-ELEMENTO     PIC X(45).
           10 5542-SAI-CINDCD-ALIQT-REDZD  PIC X(01).
           10 5542-SAI-PTX-JURO-DIA        PIC 9(03)V9(05).
           10 5542-SAI-CINDCD-FREQ-TX      PIC X(02).
           10 5542-RESERVA                 PIC X(50).

         05   5542-SAI-CONSISTENCIA.
           10 5542-SAI-TAMANHO-ERRO        PIC  9(01).
           10 5542-SAI-TABELA-ERROS OCCURS 2 TIMES.
              15 5542-SAI-CAMPOS-ERRO      PIC  9(01).

