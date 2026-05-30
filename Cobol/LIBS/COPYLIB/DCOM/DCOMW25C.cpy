      ******************************************************************
      * NOME BOOK : DCOMW25C - BOOK DE INTERFACE COM SERVICO DCOM425C  *
      * DESCRICAO : CONSULTA A TABELA TRANSITORIA PARCELA              *
      * DATA      : 16/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 631 BYTES                                          *
      ******************************************************************
       05 DCOMW25C-HEADER.
         10 DCOMW25C-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW25C'.
         10 DCOMW25C-TAM-LAYOUT        PIC 9(05)   VALUE 00631.
       05 DCOMW25C-REGISTRO.
         10 DCOMW25C-BLOCO-ENTRADA.
           15 DCOMW25C-E-DANO-OPER-DESC            PIC 9(04).
           15 DCOMW25C-E-NSEQ-OPER-DESC            PIC 9(09).
           15 DCOMW25C-E-NPCELA-DESC-COML          PIC 9(05).
         10 DCOMW25C-BLOCO-SAIDA.
           15 DCOMW25C-S-DANO-OPER-DESC            PIC 9(04).
           15 DCOMW25C-S-NSEQ-OPER-DESC            PIC 9(09).
           15 DCOMW25C-S-NPCELA-DESC-COML          PIC 9(05).
           15 DCOMW25C-S-CSIT-DESC-COML            PIC 9(03).
           15 DCOMW25C-S-HSIT-DESC-COML            PIC X(26).
           15 DCOMW25C-S-DVCTO-PCELA-DESC          PIC X(10).
           15 DCOMW25C-S-VPRINC-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25C-S-QTITLO-CHEQ-PCELA         PIC 9(09).
           15 DCOMW25C-S-PTX-JURO-MES              PIC 9(03)V9(05).
           15 DCOMW25C-S-PTX-JURO-ANO              PIC 9(03)V9(05).
           15 DCOMW25C-S-PTX-CUSTO-MES             PIC 9(03)V9(06).
           15 DCOMW25C-S-PTX-CUSTO-ANO             PIC 9(03)V9(06).
           15 DCOMW25C-S-VCUSTO-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25C-S-VJURO-PCELA-DESC          PIC 9(15)V99.
           15 DCOMW25C-S-VJURO-DIA-PCELA           PIC 9(15)V99.
           15 DCOMW25C-S-VIOF-PCELA-DESC           PIC 9(15)V99.
           15 DCOMW25C-S-VPSENT-PCELA-DESC         PIC 9(15)V99.
           15 DCOMW25C-S-VJURO-APROP-ACUM          PIC 9(15)V99.
           15 DCOMW25C-S-DULT-APROP-PCELA          PIC X(10).
           15 DCOMW25C-S-PALIQT-IOF-PRROG          PIC 9(03)V9(05).
           15 DCOMW25C-S-VIOF-PRROG-PCELA          PIC 9(15)V99.
           15 DCOMW25C-S-PCUSTO-MES-MORA           PIC 9(03)V9(06).
           15 DCOMW25C-S-PCUSTO-ANO-MORA           PIC 9(03)V9(06).
           15 DCOMW25C-S-PCUSTO-MES-CRATS          PIC 9(03)V9(06).
           15 DCOMW25C-S-PCUSTO-ANO-CRATS          PIC 9(03)V9(06).
           15 DCOMW25C-S-PALIQT-IOF-VENCD          PIC 9(03)V9(05).
           15 DCOMW25C-S-VIOF-PCELA-VENCD          PIC 9(15)V99.
           15 DCOMW25C-S-VJURO-MORA-GERC           PIC 9(15)V99.
           15 DCOMW25C-S-VJURO-MORA-LEGAL          PIC 9(15)V99.
           15 DCOMW25C-S-VSDO-DVDOR-PRINC          PIC 9(15)V99.
           15 DCOMW25C-S-VCUSTO-PCELA-VENCD        PIC 9(15)V99.
           15 DCOMW25C-S-VJURO-TRNSF-CRATS         PIC 9(15)V99.
           15 DCOMW25C-S-VCUSTO-TRNSF-CRATS        PIC 9(15)V99.
           15 DCOMW25C-S-DTRNSF-CRATS-PCELA        PIC X(10).
           15 DCOMW25C-S-DTRNSF-LP-PCELA           PIC X(10).
           15 DCOMW25C-S-CBACEN-DESC-RURAL         PIC 9(09).
           15 DCOMW25C-S-CCONTR-CORP-PCELA         PIC 9(07).
           15 DCOMW25C-S-DULT-ADTTO-PCELA          PIC X(10).
           15 DCOMW25C-S-CINDCD-MOTVO-ADTTO        PIC X(01).
           15 DCOMW25C-S-CINDCD-PEND-EXTER         PIC X(01).
           15 DCOMW25C-S-CINDCD-BLOQ-CONS          PIC X(01).
           15 DCOMW25C-S-DBLOQ-CONS-CCORR          PIC X(10).
           15 DCOMW25C-S-DULT-PRROG-PCELA          PIC X(10).
           15 DCOMW25C-S-DULT-REATV-PCELA          PIC X(10).
           15 DCOMW25C-S-HULT-ATULZ                PIC X(26).
           15 DCOMW25C-S-CFUNC-BDSCO               PIC 9(09).
           15 DCOMW25C-S-CTERM                     PIC X(08).
           15 DCOMW25C-S-DINCL-REG                 PIC X(10).
           15 DCOMW25C-S-VJURO-REMUN-VENCD         PIC 9(15)V99.
           15 DCOMW25C-S-VMORA-VENCD-DESC          PIC 9(15)V99.
           15 DCOMW25C-S-VMULTA-VENCD-DESC         PIC 9(15)V99.
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
