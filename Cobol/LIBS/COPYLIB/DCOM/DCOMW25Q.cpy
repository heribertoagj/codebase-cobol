      ******************************************************************
      * NOME BOOK : DCOMW25Q - BOOK DE INTERFACE COM SERVICO DCOM425Q  *
      * DESCRICAO : CONSULTA A TABELA HISTORICO PARCELA OPER           *
      * DATA      : 17/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 667 BYTES                                          *
      ******************************************************************
       05 DCOMW25Q-HEADER.
         10 DCOMW25Q-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW25Q'.
         10 DCOMW25Q-TAM-LAYOUT        PIC 9(05)   VALUE 00667.
       05 DCOMW25Q-REGISTRO.
         10 DCOMW25Q-BLOCO-ENTRADA.
           15 DCOMW25Q-E-DANO-OPER-DESC            PIC 9(04).
           15 DCOMW25Q-E-NSEQ-OPER-DESC            PIC 9(09).
           15 DCOMW25Q-E-NPCELA-DESC-COML          PIC 9(05).
           15 DCOMW25Q-E-HULT-ATULZ                PIC X(26).
         10 DCOMW25Q-BLOCO-SAIDA.
           15 DCOMW25Q-S-DANO-OPER-DESC            PIC 9(04).
           15 DCOMW25Q-S-NSEQ-OPER-DESC            PIC 9(09).
           15 DCOMW25Q-S-NPCELA-DESC-COML          PIC 9(05).
           15 DCOMW25Q-S-HULT-ATULZ                PIC X(26).
           15 DCOMW25Q-S-DMES-VCTO-PTCAO           PIC 9(02).
           15 DCOMW25Q-S-CSIT-DESC-COML            PIC 9(03).
           15 DCOMW25Q-S-HSIT-DESC-COML            PIC X(26).
           15 DCOMW25Q-S-DVCTO-PCELA-DESC          PIC X(10).
           15 DCOMW25Q-S-VPRINC-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25Q-S-QTITLO-CHEQ-PCELA         PIC 9(09).
           15 DCOMW25Q-S-CBCO-DEPOS                PIC 9(03).
           15 DCOMW25Q-S-CAG-BCRIA-DEPOS           PIC 9(05).
           15 DCOMW25Q-S-PTX-JURO-MES              PIC 9(03)V9(05).
           15 DCOMW25Q-S-PTX-JURO-ANO              PIC 9(03)V9(05).
           15 DCOMW25Q-S-PTX-CUSTO-MES             PIC 9(03)V9(06).
           15 DCOMW25Q-S-PTX-CUSTO-ANO             PIC 9(03)V9(06).
           15 DCOMW25Q-S-VCUSTO-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25Q-S-VJURO-PCELA-DESC          PIC 9(15)V99.
           15 DCOMW25Q-S-VJURO-DIA-PCELA           PIC 9(15)V99.
           15 DCOMW25Q-S-VIOF-PCELA-DESC           PIC 9(15)V99.
           15 DCOMW25Q-S-VPSENT-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25Q-S-VJURO-APROP-ACUM          PIC 9(15)V99.
           15 DCOMW25Q-S-DULT-APROP-PCELA          PIC X(10).
           15 DCOMW25Q-S-PALIQT-IOF-PRROG          PIC 9(03)V9(05).
           15 DCOMW25Q-S-VIOF-PRROG-PCELA          PIC 9(15)V99.
           15 DCOMW25Q-S-PCUSTO-MES-MORA           PIC 9(03)V9(06).
           15 DCOMW25Q-S-PCUSTO-ANO-MORA           PIC 9(03)V9(06).
           15 DCOMW25Q-S-PCUSTO-MES-CRATS          PIC 9(03)V9(06).
           15 DCOMW25Q-S-PCUSTO-ANO-CRATS          PIC 9(03)V9(06).
           15 DCOMW25Q-S-PALIQT-IOF-VENCD          PIC 9(03)V9(05).
           15 DCOMW25Q-S-VIOF-PCELA-VENCD          PIC 9(15)V99.
           15 DCOMW25Q-S-VJURO-MORA-GERC           PIC 9(15)V99.
           15 DCOMW25Q-S-VJURO-MORA-LEGAL          PIC 9(15)V99.
           15 DCOMW25Q-S-VSDO-DVDOR-PRINC          PIC 9(15)V99.
           15 DCOMW25Q-S-VCUSTO-PCELA-VENCD        PIC 9(15)V99.
           15 DCOMW25Q-S-VJURO-TRNSF-CRATS         PIC 9(15)V99.
           15 DCOMW25Q-S-VCUSTO-TRNSF-CRATS        PIC 9(15)V99.
           15 DCOMW25Q-S-DTRNSF-CRATS-PCELA        PIC X(10).
           15 DCOMW25Q-S-DTRNSF-LP-PCELA           PIC X(10).
           15 DCOMW25Q-S-CREFT-BACEN               PIC 9(09).
           15 DCOMW25Q-S-CCONTR-CORP-PCELA         PIC 9(07).
           15 DCOMW25Q-S-DULT-ADTTO-PCELA          PIC X(10).
           15 DCOMW25Q-S-CINDCD-MOTVO-ADTTO        PIC X(01).
           15 DCOMW25Q-S-CINDCD-PEND-EXTER         PIC X(01).
           15 DCOMW25Q-S-CINDCD-BLOQ-CONS          PIC X(01).
           15 DCOMW25Q-S-DBLOQ-CONS-CCORR          PIC X(10).
           15 DCOMW25Q-S-DULT-PRROG-PCELA          PIC X(10).
           15 DCOMW25Q-S-DULT-REATV-PCELA          PIC X(10).
           15 DCOMW25Q-S-CFUNC-BDSCO               PIC 9(09).
           15 DCOMW25Q-S-CTERM                     PIC X(08).
           15 DCOMW25Q-S-DINCL-REG                 PIC X(10).
           15 DCOMW25Q-S-VJURO-REMUN-VENCD         PIC 9(15)V99.
           15 DCOMW25Q-S-VMORA-VENCD-DESC          PIC 9(15)V99.
           15 DCOMW25Q-S-VMULTA-VENCD-DESC         PIC 9(15)V99.
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
