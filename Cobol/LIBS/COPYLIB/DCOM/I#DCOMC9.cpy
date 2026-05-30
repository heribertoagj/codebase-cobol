      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C9 - DB2PRD.TPCELA_OPER_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                 *
BSI001*---------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA    *
BSI001*  DATA........:   28/04/2017                                   *
BSI001*---------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.       *
BSI001*****************************************************************
       01  C9-ARGUMENTOS-ENTRADA.

       05  C9-COLUNAS-DA-TABELA.
           10  C9-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C9-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  C9-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  C9-DMES-VCTO-PTCAO              PIC S9(2)V COMP-3.
           10  C9-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  C9-HSIT-DESC-COML               PIC X(26).
           10  C9-DVCTO-PCELA-DESC             PIC X(10).
           10  C9-VPRINC-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  C9-QTITLO-CHEQ-PCELA            PIC S9(9)V COMP-3.
           10  C9-CBCO-DEPOS                   PIC S9(3)V COMP-3.
           10  C9-CAG-BCRIA-DEPOS              PIC S9(5)V COMP-3.
           10  C9-PTX-JURO-MES                 PIC S9(3)V9(5) COMP-3.
           10  C9-PTX-JURO-ANO                 PIC S9(3)V9(5) COMP-3.
           10  C9-PTX-CUSTO-MES                PIC S9(3)V9(6) COMP-3.
           10  C9-PTX-CUSTO-ANO                PIC S9(3)V9(6) COMP-3.
           10  C9-VCUSTO-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  C9-VJURO-PCELA-DESC             PIC S9(15)V9(2) COMP-3.
           10  C9-VJURO-DIA-PCELA              PIC S9(15)V9(2) COMP-3.
           10  C9-VIOF-PCELA-DESC              PIC S9(15)V9(2) COMP-3.
           10  C9-VPSENT-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  C9-VJURO-APROP-ACUM             PIC S9(15)V9(2) COMP-3.
           10  C9-DULT-APROP-PCELA             PIC X(10).
           10  C9-PALIQT-IOF-PRROG             PIC S9(3)V9(5) COMP-3.
           10  C9-VIOF-PRROG-PCELA             PIC S9(15)V9(2) COMP-3.
           10  C9-PCUSTO-MES-MORA              PIC S9(3)V9(6) COMP-3.
           10  C9-PCUSTO-ANO-MORA              PIC S9(3)V9(6) COMP-3.
           10  C9-PCUSTO-MES-CRATS             PIC S9(3)V9(6) COMP-3.
           10  C9-PCUSTO-ANO-CRATS             PIC S9(3)V9(6) COMP-3.
           10  C9-PALIQT-IOF-VENCD             PIC S9(3)V9(5) COMP-3.
           10  C9-VIOF-PCELA-VENCD             PIC S9(15)V9(2) COMP-3.
           10  C9-VJURO-MORA-GERC              PIC S9(15)V9(2) COMP-3.
           10  C9-VJURO-MORA-LEGAL             PIC S9(15)V9(2) COMP-3.
           10  C9-VSDO-DVDOR-PRINC             PIC S9(15)V9(2) COMP-3.
           10  C9-VCUSTO-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  C9-VJURO-TRNSF-CRATS            PIC S9(15)V9(2) COMP-3.
           10  C9-VCUSTO-TRNSF-CRATS           PIC S9(15)V9(2) COMP-3.
           10  C9-DTRNSF-CRATS-PCELA           PIC X(10).
           10  C9-DTRNSF-LP-PCELA              PIC X(10).
           10  C9-CBACEN-DESC-RURAL            PIC S9(9)V COMP-3.
           10  C9-CCONTR-CORP-PCELA            PIC S9(7)V COMP-3.
           10  C9-DULT-ADTTO-PCELA             PIC X(10).
           10  C9-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  C9-CINDCD-PEND-EXTER            PIC X(1).
           10  C9-CINDCD-BLOQ-CONS             PIC X(1).
           10  C9-DBLOQ-CONS-CCORR             PIC X(10).
           10  C9-DULT-PRROG-PCELA             PIC X(10).
           10  C9-DULT-REATV-PCELA             PIC X(10).
           10  C9-HULT-ATULZ                   PIC X(26).
           10  C9-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C9-CTERM                        PIC X(8).
           10  C9-DINCL-REG                    PIC X(10).
BSI001     10  C9-VJURO-REMUN-VENCD            PIC S9(15)V9(2) COMP-3.
BSI001     10  C9-VMORA-VENCD-DESC             PIC S9(15)V9(2) COMP-3.
BSI001     10  C9-VMULTA-VENCD-DESC            PIC S9(15)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C9-RETORNO.
       05  C9-COD-RETORNO                         PIC 9(04).
       05  C9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C9-PROXIMO-RESTART                     PIC 9(05).
       05  C9-QTDE-RETORNADA                      PIC 9(03).
       05  C9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC9.
           10  C9RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C9RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  C9RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  C9RT-DMES-VCTO-PTCAO            PIC S9(2)V COMP-3.
           10  C9RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  C9RT-HSIT-DESC-COML             PIC X(26).
           10  C9RT-DVCTO-PCELA-DESC           PIC X(10).
           10  C9RT-VPRINC-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  C9RT-QTITLO-CHEQ-PCELA          PIC S9(9)V COMP-3.
           10  C9RT-CBCO-DEPOS                 PIC S9(3)V COMP-3.
           10  C9RT-CAG-BCRIA-DEPOS            PIC S9(5)V COMP-3.
           10  C9RT-PTX-JURO-MES               PIC S9(3)V9(5) COMP-3.
           10  C9RT-PTX-JURO-ANO               PIC S9(3)V9(5) COMP-3.
           10  C9RT-PTX-CUSTO-MES              PIC S9(3)V9(6) COMP-3.
           10  C9RT-PTX-CUSTO-ANO              PIC S9(3)V9(6) COMP-3.
           10  C9RT-VCUSTO-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  C9RT-VJURO-PCELA-DESC           PIC S9(15)V9(2) COMP-3.
           10  C9RT-VJURO-DIA-PCELA            PIC S9(15)V9(2) COMP-3.
           10  C9RT-VIOF-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  C9RT-VPSENT-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  C9RT-VJURO-APROP-ACUM           PIC S9(15)V9(2) COMP-3.
           10  C9RT-DULT-APROP-PCELA           PIC X(10).
           10  C9RT-PALIQT-IOF-PRROG           PIC S9(3)V9(5) COMP-3.
           10  C9RT-VIOF-PRROG-PCELA           PIC S9(15)V9(2) COMP-3.
           10  C9RT-PCUSTO-MES-MORA            PIC S9(3)V9(6) COMP-3.
           10  C9RT-PCUSTO-ANO-MORA            PIC S9(3)V9(6) COMP-3.
           10  C9RT-PCUSTO-MES-CRATS           PIC S9(3)V9(6) COMP-3.
           10  C9RT-PCUSTO-ANO-CRATS           PIC S9(3)V9(6) COMP-3.
           10  C9RT-PALIQT-IOF-VENCD           PIC S9(3)V9(5) COMP-3.
           10  C9RT-VIOF-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  C9RT-VJURO-MORA-GERC            PIC S9(15)V9(2) COMP-3.
           10  C9RT-VJURO-MORA-LEGAL           PIC S9(15)V9(2) COMP-3.
           10  C9RT-VSDO-DVDOR-PRINC           PIC S9(15)V9(2) COMP-3.
           10  C9RT-VCUSTO-PCELA-VENCD         PIC S9(15)V9(2) COMP-3.
           10  C9RT-VJURO-TRNSF-CRATS          PIC S9(15)V9(2) COMP-3.
           10  C9RT-VCUSTO-TRNSF-CRATS         PIC S9(15)V9(2) COMP-3.
           10  C9RT-DTRNSF-CRATS-PCELA         PIC X(10).
           10  C9RT-DTRNSF-LP-PCELA            PIC X(10).
           10  C9RT-CBACEN-DESC-RURAL          PIC S9(9)V COMP-3.
           10  C9RT-CCONTR-CORP-PCELA          PIC S9(7)V COMP-3.
           10  C9RT-DULT-ADTTO-PCELA           PIC X(10).
           10  C9RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  C9RT-CINDCD-PEND-EXTER          PIC X(1).
           10  C9RT-CINDCD-BLOQ-CONS           PIC X(1).
           10  C9RT-DBLOQ-CONS-CCORR           PIC X(10).
           10  C9RT-DULT-PRROG-PCELA           PIC X(10).
           10  C9RT-DULT-REATV-PCELA           PIC X(10).
           10  C9RT-HULT-ATULZ                 PIC X(26).
           10  C9RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C9RT-CTERM                      PIC X(8).
           10  C9RT-DINCL-REG                  PIC X(10).
BSI001     10  C9RT-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001     10  C9RT-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001     10  C9RT-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
