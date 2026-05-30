      ******************************************************************
      * NOME BOOK : DCOMW25D - BOOK DE INTERFACE COM SERVICO DCOM425D  *
      * DESCRICAO : CONSULTA A TABELA OFICIAL DE PARCELAS              *
      * DATA      : 16/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 641 BYTES                                          *
      ******************************************************************
       05 DCOMW25D-HEADER.
         10 DCOMW25D-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW25D'.
         10 DCOMW25D-TAM-LAYOUT        PIC 9(05)   VALUE 00641.
       05 DCOMW25D-REGISTRO.
         10 DCOMW25D-BLOCO-ENTRADA.
           15 DCOMW25D-E-DANO-OPER-DESC            PIC 9(04).
           15 DCOMW25D-E-NSEQ-OPER-DESC            PIC 9(09).
           15 DCOMW25D-E-NPCELA-DESC-COML          PIC 9(05).
         10 DCOMW25D-BLOCO-SAIDA.
           15 DCOMW25D-S-DANO-OPER-DESC            PIC 9(04).
           15 DCOMW25D-S-NSEQ-OPER-DESC            PIC 9(09).
           15 DCOMW25D-S-NPCELA-DESC-COML          PIC 9(05).
           15 DCOMW25D-S-DMES-VCTO-PTCAO           PIC 9(02).
           15 DCOMW25D-S-CSIT-DESC-COML            PIC 9(03).
           15 DCOMW25D-S-HSIT-DESC-COML            PIC X(26).
           15 DCOMW25D-S-DVCTO-PCELA-DESC          PIC X(10).
           15 DCOMW25D-S-VPRINC-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25D-S-QTITLO-CHEQ-PCELA         PIC 9(09).
           15 DCOMW25D-S-CBCO-DEPOS                PIC 9(03).
           15 DCOMW25D-S-CAG-BCRIA-DEPOS           PIC 9(05).
           15 DCOMW25D-S-PTX-JURO-MES              PIC 9(03)V9(05).
           15 DCOMW25D-S-PTX-JURO-ANO              PIC 9(03)V9(05).
           15 DCOMW25D-S-PTX-CUSTO-MES             PIC 9(03)V9(06).
           15 DCOMW25D-S-PTX-CUSTO-ANO             PIC 9(03)V9(06).
           15 DCOMW25D-S-VCUSTO-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25D-S-VJURO-PCELA-DESC          PIC 9(15)V99.
           15 DCOMW25D-S-VJURO-DIA-PCELA           PIC 9(15)V99.
           15 DCOMW25D-S-VIOF-PCELA-DESC           PIC 9(15)V99.
           15 DCOMW25D-S-VPSENT-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25D-S-VJURO-APROP-ACUM          PIC 9(15)V99.
           15 DCOMW25D-S-DULT-APROP-PCELA          PIC X(10).
           15 DCOMW25D-S-PALIQT-IOF-PRROG          PIC 9(03)V9(05).
           15 DCOMW25D-S-VIOF-PRROG-PCELA          PIC 9(15)V99.
           15 DCOMW25D-S-PCUSTO-MES-MORA           PIC 9(03)V9(06).
           15 DCOMW25D-S-PCUSTO-ANO-MORA           PIC 9(03)V9(06).
           15 DCOMW25D-S-PCUSTO-MES-CRATS          PIC 9(03)V9(06).
           15 DCOMW25D-S-PCUSTO-ANO-CRATS          PIC 9(03)V9(06).
           15 DCOMW25D-S-PALIQT-IOF-VENCD          PIC 9(03)V9(05).
           15 DCOMW25D-S-VIOF-PCELA-VENCD          PIC 9(15)V99.
           15 DCOMW25D-S-VJURO-MORA-GERC           PIC 9(15)V99.
           15 DCOMW25D-S-VJURO-MORA-LEGAL          PIC 9(15)V99.
           15 DCOMW25D-S-VSDO-DVDOR-PRINC          PIC 9(15)V99.
           15 DCOMW25D-S-VCUSTO-PCELA-VENCD        PIC 9(15)V99.
           15 DCOMW25D-S-VJURO-TRNSF-CRATS         PIC 9(15)V99.
           15 DCOMW25D-S-VCUSTO-TRNSF-CRATS        PIC 9(15)V99.
           15 DCOMW25D-S-DTRNSF-CRATS-PCELA        PIC X(10).
           15 DCOMW25D-S-DTRNSF-LP-PCELA           PIC X(10).
           15 DCOMW25D-S-CBACEN-DESC-RURAL         PIC 9(09).
           15 DCOMW25D-S-CCONTR-CORP-PCELA         PIC 9(07).
           15 DCOMW25D-S-DULT-ADTTO-PCELA          PIC X(10).
           15 DCOMW25D-S-CINDCD-MOTVO-ADTTO        PIC X(01).
           15 DCOMW25D-S-CINDCD-PEND-EXTER         PIC X(01).
           15 DCOMW25D-S-CINDCD-BLOQ-CONS          PIC X(01).
           15 DCOMW25D-S-DBLOQ-CONS-CCORR          PIC X(10).
           15 DCOMW25D-S-DULT-PRROG-PCELA          PIC X(10).
           15 DCOMW25D-S-DULT-REATV-PCELA          PIC X(10).
           15 DCOMW25D-S-HULT-ATULZ                PIC X(26).
           15 DCOMW25D-S-CFUNC-BDSCO               PIC 9(09).
           15 DCOMW25D-S-CTERM                     PIC X(08).
           15 DCOMW25D-S-DINCL-REG                 PIC X(10).
           15 DCOMW25D-S-VJURO-REMUN-VENCD         PIC 9(15)V99.
           15 DCOMW25D-S-VMORA-VENCD-DESC          PIC 9(15)V99.
           15 DCOMW25D-S-VMULTA-VENCD-DESC         PIC 9(15)V99.
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
