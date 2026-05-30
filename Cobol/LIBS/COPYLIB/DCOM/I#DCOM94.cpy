      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB094 - DB2PRD.TBAIXA_PCELA_DESC      *
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
BRQ001*                    ALTERACAO - BRQ INFORMATICA                *
BRQ001*---------------------------------------------------------------*
BRQ001*  ANALISTA BRQ:   FABIO AUGUSTO FINK      -  BRQ INFORMATICA   *
BRQ001*  DATA........:   08/02/2018                                   *
BRQ001*---------------------------------------------------------------*
BRQ001*  OBJETIVO....:   PROJETO BRQ 17/0418 - TROCAR O CAMPO:        *
BRQ001*            DE: 'CFORMA-PGTO-TITLO' / PARA: 'CORIGE-PGTO-DESC' *
BRQ001*****************************************************************
       01  94-ARGUMENTOS-ENTRADA.

       05  94-COLUNAS-DA-TABELA.
           10  94-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  94-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  94-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  94-HBAIXA-PCELA-DESC            PIC X(26).
           10  94-DMES-BAIXA-PTCAO             PIC S9(2)V COMP-3.
           10  94-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  94-HSIT-DESC-COML               PIC X(26).
           10  94-CTPO-BAIXA-PCELA             PIC X(1).
           10  94-CMEIO-PGTO-DESC              PIC S9(3)V COMP-3.
           10  94-DEFETV-BAIXA-PCELA           PIC X(10).
           10  94-VSDO-DVDOR-PCELA             PIC S9(15)V9(2) COMP-3.
           10  94-VPRINC-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  94-VJURO-BAIXA-PCELA            PIC S9(15)V9(2) COMP-3.
           10  94-VIOF-BAIXA-PCELA             PIC S9(15)V9(2) COMP-3.
           10  94-VMULTA-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  94-VABTMT-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  94-VDESC-BAIXA-PCELA            PIC S9(15)V9(2) COMP-3.
           10  94-VEFETV-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  94-VJURO-CUSTO-BAIXA            PIC S9(15)V9(2) COMP-3.
           10  94-VMORA-LEGAL-BAIXA            PIC S9(15)V9(2) COMP-3.
           10  94-CINDCD-LOC-DEB               PIC X(1).
           10  94-CINDCD-BAIXA-COBR            PIC X(1).
           10  94-CINDCD-CONSL-EXTRT           PIC X(1).
BRQ001     10  94-CORIGE-PGTO-DESC             PIC S9(3)V COMP-3.
           10  94-CMIDIA-PGTO-TITLO            PIC S9(5)V COMP-3.
           10  94-CBCO                         PIC S9(3)V COMP-3.
           10  94-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  94-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  94-CDIG-CTA-EXTER               PIC X(2).
           10  94-CCTRO-CUSTO                  PIC X(4).
           10  94-HULT-ATULZ                   PIC X(26).
           10  94-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  94-CTERM                        PIC X(8).
           10  94-CTPO-BAIXA-CONTR             PIC X(1).
           10  94-CTPO-BAIXA-COMPL             PIC X(1).
           10  94-VDEVLC-BAIXA-EXCED           PIC S9(15)V9(2) COMP-3.
           10  94-VRECTA-BAIXA-EXCED           PIC S9(15)V9(2) COMP-3.
           10  94-CBCO-CHEQC                   PIC S9(3)V COMP-3.
           10  94-CAG-BCRIA-CHEQC              PIC S9(5)V COMP-3.
           10  94-CCCORR-CHEQC                 PIC S9(13)V COMP-3.
           10  94-CCHEQ-CHEQC                  PIC S9(7)V COMP-3.
BSI001     10  94-VJURO-REMUN-VENCD            PIC S9(15)V9(2) COMP-3.
BSI001     10  94-VMORA-VENCD-DESC             PIC S9(15)V9(2) COMP-3.
BSI001     10  94-VMULTA-VENCD-DESC            PIC S9(15)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  94-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  94-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  94-RETORNO.
       05  94-COD-RETORNO                         PIC 9(04).
       05  94-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  94-PROXIMO-RESTART                     PIC 9(05).
       05  94-QTDE-RETORNADA                      PIC 9(03).
       05  94-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  94-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T94.
           10  94RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  94RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  94RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  94RT-HBAIXA-PCELA-DESC          PIC X(26).
           10  94RT-DMES-BAIXA-PTCAO           PIC S9(2)V COMP-3.
           10  94RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  94RT-HSIT-DESC-COML             PIC X(26).
           10  94RT-CTPO-BAIXA-PCELA           PIC X(1).
           10  94RT-CMEIO-PGTO-DESC            PIC S9(3)V COMP-3.
           10  94RT-DEFETV-BAIXA-PCELA         PIC X(10).
           10  94RT-VSDO-DVDOR-PCELA           PIC S9(15)V9(2) COMP-3.
           10  94RT-VPRINC-BAIXA-PCELA         PIC S9(15)V9(2) COMP-3.
           10  94RT-VJURO-BAIXA-PCELA          PIC S9(15)V9(2) COMP-3.
           10  94RT-VIOF-BAIXA-PCELA           PIC S9(15)V9(2) COMP-3.
           10  94RT-VMULTA-BAIXA-PCELA         PIC S9(15)V9(2) COMP-3.
           10  94RT-VABTMT-BAIXA-PCELA         PIC S9(15)V9(2) COMP-3.
           10  94RT-VDESC-BAIXA-PCELA          PIC S9(15)V9(2) COMP-3.
           10  94RT-VEFETV-BAIXA-PCELA         PIC S9(15)V9(2) COMP-3.
           10  94RT-VJURO-CUSTO-BAIXA          PIC S9(15)V9(2) COMP-3.
           10  94RT-VMORA-LEGAL-BAIXA          PIC S9(15)V9(2) COMP-3.
           10  94RT-CINDCD-LOC-DEB             PIC X(1).
           10  94RT-CINDCD-BAIXA-COBR          PIC X(1).
           10  94RT-CINDCD-CONSL-EXTRT         PIC X(1).
BRQ001     10  94RT-CORIGE-PGTO-DESC           PIC S9(3)V COMP-3.
           10  94RT-CMIDIA-PGTO-TITLO          PIC S9(5)V COMP-3.
           10  94RT-CBCO                       PIC S9(3)V COMP-3.
           10  94RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  94RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  94RT-CDIG-CTA-EXTER             PIC X(2).
           10  94RT-CCTRO-CUSTO                PIC X(4).
           10  94RT-HULT-ATULZ                 PIC X(26).
           10  94RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  94RT-CTERM                      PIC X(8).
           10  94RT-CTPO-BAIXA-CONTR           PIC X(1).
           10  94RT-CTPO-BAIXA-COMPL           PIC X(1).
           10  94RT-VDEVLC-BAIXA-EXCED         PIC S9(15)V9(2) COMP-3.
           10  94RT-VRECTA-BAIXA-EXCED         PIC S9(15)V9(2) COMP-3.
           10  94RT-CBCO-CHEQC                 PIC S9(3)V COMP-3.
           10  94RT-CAG-BCRIA-CHEQC            PIC S9(5)V COMP-3.
           10  94RT-CCCORR-CHEQC               PIC S9(13)V COMP-3.
           10  94RT-CCHEQ-CHEQC                PIC S9(7)V COMP-3.
BSI001     10  94RT-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001     10  94RT-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001     10  94RT-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
