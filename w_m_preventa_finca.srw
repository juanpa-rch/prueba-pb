$PBExportHeader$w_m_preventa_finca.srw
$PBExportComments$Preventas nuevo
forward
global type w_m_preventa_finca from w_cabe_deta
end type
type cb_copy from commandbutton within w_m_preventa_finca
end type
type dw_imprimir from datawindow within w_m_preventa_finca
end type
type dw_deta from uo_control_det within w_m_preventa_finca
end type
type dw_1 from uo_control_cabe within w_m_preventa_finca
end type
end forward

shared variables

end variables

global type w_m_preventa_finca from w_cabe_deta
integer width = 4480
integer height = 2180
string title = "Preventa finca"
string menuname = "m_menu_main_ingreso_d2"
event ue_grabar_det pbm_custom69
event ue_inventario pbm_custom16
cb_copy cb_copy
dw_imprimir dw_imprimir
dw_deta dw_deta
dw_1 dw_1
end type
global w_m_preventa_finca w_m_preventa_finca

type variables
Boolean ib_grabar, ib_aceptar
Long il_factura, il_fila_actual, il_modificar


Private:
	nvo_ramos iramos
	nvo_parametros_floricolas iparametros_floricolas
	Long il_puntocorte
	Long il_empresa, il_cliente
	
	String is_manejaFlorComprada
	String is_origenFlor = 'F'

	Long il_codigoFinca
	
	nvo_validainformacion ivalidainformacion	// 16-05-2018

	// Flete
	nvo_flete iflete	// 09-07-2018

end variables

forward prototypes
public subroutine wf_actualiza_totales (integer ai_fila)
public subroutine wf_crea_ramo ()
public subroutine wf_llena_detalles (integer ai_fila)
public subroutine wf_numera_filas ()
public subroutine wf_renumerar ()
public subroutine wf_filtraciudad ()
public subroutine wf_filtracuartofrio (integer ai_valordefault)
public subroutine wf_filtra_subclientes ()
public function integer wf_validacaja (long al_codigocajaorigen, long al_codigocaja)
public subroutine wf_filtra_notify (integer ai_default)
public subroutine wf_actualizapuntocortedetalle ()
public subroutine wf_filtrarvariedades (long al_codigofinca)
public subroutine wf_suprimeopciones ()
public subroutine wf_filtracajamarcada ()
end prototypes

public subroutine wf_actualiza_totales (integer ai_fila);Long ll_cantidad, ll_tallosxramo, ll_numerocajas, ll_tallos
Double ldo_precio
dwItemStatus l_status	// 16-05-2018
Long ll_cont	// 09-07-2018


IF ai_fila > 0 THEN
	l_status = idw_detalles.GetItemStatus(ai_fila, 0, Primary!)
	IF l_status = DataModified! OR l_status = NewModified! THEN
		ll_cantidad		= idw_detalles.GetItemNumber(ai_fila, 'drso_can_drso')
		ldo_precio		= idw_detalles.GetItemNumber(ai_fila, 'drso_pre_tall')
		ll_tallosxramo = idw_detalles.GetItemNumber(ai_fila, 'drso_cod_ntal')
		ll_numerocajas = idw_detalles.GetItemNumber(ai_fila, 'drso_num_caja')
		ll_tallos = ll_cantidad * ll_numerocajas * ll_tallosxramo
		
		idw_detalles.SetItem(ai_fila, "drso_sub_tota", Round(ll_tallos * ldo_precio, 2))
		idw_detalles.SetItem(ai_fila, "drso_tot_tall", Round(ll_tallos, 0))
		idw_detalles.SetItem(ai_fila, "drso_tot_ramo", Round(ll_cantidad * ll_numerocajas, 0))
	END IF
ELSE
	FOR ll_cont = 1 TO idw_detalles.RowCount()
		l_status = idw_detalles.GetItemStatus(ll_cont, 0, Primary!)
		IF l_status = DataModified! OR l_status = NewModified! THEN
			ll_cantidad		= idw_detalles.GetItemNumber(ll_cont, 'drso_can_drso')
			ldo_precio		= idw_detalles.GetItemNumber(ll_cont, 'drso_pre_tall')
			ll_tallosxramo = idw_detalles.GetItemNumber(ll_cont, 'drso_cod_ntal')
			ll_numerocajas = idw_detalles.GetItemNumber(ll_cont, 'drso_num_caja')
			ll_tallos = ll_cantidad * ll_numerocajas * ll_tallosxramo
			
			idw_detalles.SetItem(ll_cont, "drso_sub_tota", Round(ll_tallos * ldo_precio, 2))
			idw_detalles.SetItem(ll_cont, "drso_tot_tall", Round(ll_tallos, 0))
			idw_detalles.SetItem(ll_cont, "drso_tot_ramo", Round(ll_cantidad * ll_numerocajas, 0))
		END IF
	NEXT
END IF

end subroutine

public subroutine wf_crea_ramo ();String ls_variedad, ls_ramo
Long ll_grado, ll_tallosxramo
nvo_ramos lramos
nvo_transaccion ltransaccion
Long ll_ramos, ll_codigoEspecie


IF idw_detalles.GetRow() > 0 THEN
	idw_detalles.AcceptText()
	
	ls_variedad    = idw_detalles.GetItemString(idw_detalles.GetRow(), 'drso_cod_vard')
	ll_grado       = idw_detalles.GetItemNumber(idw_detalles.GetRow(), 'drso_num_grad')
	ll_tallosxramo = idw_detalles.GetItemNumber(idw_detalles.GetRow(), 'drso_cod_ntal')
	il_puntocorte  = idw_control.GetItemNumber(idw_control.GetRow(), 'sord_cod_ptco')
	
	ls_ramo = lramos.of_creacodigoramo(ls_variedad, ll_grado, ll_tallosxramo, il_puntocorte)
	idw_detalles.SetItem(idw_detalles.GetRow(), 'drso_cod_ramo', ls_ramo)	

	IF NOT IsNull(ls_ramo) THEN
		// Creo el ramo si no existe
		ll_ramos = gf_validar_ramo(ls_ramo)	// 30-06-2017
		
		IF ll_ramos = 0 THEN
			SELECT vard_cod_espe INTO :ll_codigoEspecie
			FROM saevard
			WHERE vard_cod_vard = :ls_variedad
			AND vard_cod_finc = :gi_finca;
			
			ltransaccion = Create nvo_transaccion
			ltransaccion.of_conectar_bdd( )
			
			INSERT INTO saeramo VALUES (:ls_ramo, :gi_finca, 1, :ll_grado, &
				:il_puntocorte, :ls_variedad, :ll_tallosxramo, &
				:ll_codigoEspecie, null, :gnv_ctrdw.il_empresa) USING ltransaccion;
				
			ltransaccion.of_procesar_transaccion(0)
			ltransaccion.of_desconectar_bdd( )
		END IF
		// FIN Creo el ramo si no existe
	END IF
END IF

end subroutine

public subroutine wf_llena_detalles (integer ai_fila);Long ll_cont, ll_caja_ini, ll_caja_fin, ll_numero_cajas
Long ll_caja_ini_d, ll_marca, ll_codigoCaja
String ls_tipolinea_d


IF ai_fila > 0 THEN
	idw_detalles.SetReDraw(False)
	ll_caja_ini		= idw_detalles.GetitemNumber(ai_fila, 'drso_caj_inic')
	ll_caja_fin		= idw_detalles.GetitemNumber(ai_fila, 'drso_caj_fina')
	ll_numero_cajas= idw_detalles.GetitemNumber(ai_fila, 'drso_num_caja')
	ll_marca			= idw_detalles.GetitemNumber(ai_fila, 'drso_cod_cmar')
	ll_codigoCaja	= idw_detalles.GetitemNumber(ai_fila, 'drso_cod_caja')
	
	FOR ll_cont = ai_fila + 1 TO idw_detalles.RowCount()
		ll_caja_ini_d	= idw_detalles.GetitemNumber(ll_cont, 'drso_caj_inic')
		ls_tipolinea_d	= idw_detalles.GetitemString(ll_cont, 'drso_pod_drso')
		
		IF ll_caja_ini = ll_caja_ini_d AND &
			ls_tipolinea_d = 'D' THEN
			idw_detalles.SetItem(ll_cont, 'drso_caj_fina', ll_caja_fin)
			idw_detalles.SetItem(ll_cont, 'drso_num_caja', ll_numero_cajas)
			idw_detalles.SetItem(ll_cont, 'drso_cod_cmar', ll_marca)
			idw_detalles.SetItem(ll_cont, 'drso_cod_caja', ll_codigoCaja)
			wf_actualiza_totales(ll_cont)
		ELSE
			Exit
		END IF
	NEXT
	idw_detalles.SetReDraw(True)
END IF

end subroutine

public subroutine wf_numera_filas ();Long ll_cont


FOR ll_cont = 1 TO idw_detalles.RowCount()
	idw_detalles.SetReDraw(False)
	idw_detalles.SetItem(ll_cont, 'drso_ord_drso', ll_cont)
	idw_detalles.SetReDraw(True)
NEXT

end subroutine

public subroutine wf_renumerar ();Long ll_cajainicial, ll_cont, ll_codigocaja, ll_cajafinal
String ls_tipolinea, ls_tipocaja[]
Double ldo_numerocajas, ldo_numerocajas_ant
nvo_funciones_facturacion lfunciones_facturacion


ll_cajainicial = 1
idw_detalles.SetReDraw(False)
FOR ll_cont = 1 TO idw_detalles.RowCount()
	// lee
	ls_tipolinea	= idw_detalles.GetItemString(ll_cont, 'drso_pod_drso')
	ldo_numerocajas= idw_detalles.GetItemNumber(ll_cont, 'drso_num_caja')
	ll_codigocaja	= idw_detalles.GetItemNumber(ll_cont, 'drso_cod_caja')
	
	IF ll_cont = 1 THEN
		ll_cajafinal = ldo_numerocajas
		ls_tipolinea = 'P'
	ELSE
		ls_tipocaja[ll_cont]	= lfunciones_facturacion.of_gettipocaja(ll_codigocaja)
		CHOOSE CASE ls_tipocaja[ll_cont]
			CASE 'S'
				ll_cajainicial = idw_detalles.GetItemNumber(ll_cont - 1, 'drso_caj_fina') + 1
				ll_cajafinal	= idw_detalles.GetItemNumber(ll_cont - 1, 'drso_caj_fina') + ldo_numerocajas
				ls_tipolinea = 'P'
			CASE 'M'
				IF ls_tipocaja[ll_cont - 1] = 'S' THEN
					ll_cajainicial = idw_detalles.GetItemNumber(ll_cont - 1, 'drso_caj_fina') + 1
					ll_cajafinal	= idw_detalles.GetItemNumber(ll_cont - 1, 'drso_caj_fina') + ldo_numerocajas
					ls_tipolinea = 'P'
				ELSE
					ldo_numerocajas_ant	= idw_detalles.GetItemNumber(ll_cont - 1, 'drso_num_caja')
					ls_tipolinea			= idw_detalles.GetItemString(ll_cont, 'drso_pod_drso')
					IF ldo_numerocajas = ldo_numerocajas_ant AND ls_tipolinea <> 'P' THEN
						ll_cajainicial = idw_detalles.GetItemNumber(ll_cont - 1, 'drso_caj_inic')
						ll_cajafinal	= idw_detalles.GetItemNumber(ll_cont - 1, 'drso_caj_fina')
						ls_tipolinea = 'D'
					ELSE
						ll_cajainicial = idw_detalles.GetItemNumber(ll_cont - 1, 'drso_caj_fina') + 1
						ll_cajafinal	= idw_detalles.GetItemNumber(ll_cont - 1, 'drso_caj_fina') + ldo_numerocajas
						ls_tipolinea = 'P'
					END IF
				END IF
		END CHOOSE
	END IF
	
	// Llena numero de cajas
	idw_detalles.SetItem(ll_cont, 'drso_ord_drso', ll_cont)
	idw_detalles.SetItem(ll_cont, 'drso_caj_inic', ll_cajainicial)
	idw_detalles.SetItem(ll_cont, 'drso_caj_fina', ll_cajafinal)
	idw_detalles.SetItem(ll_cont, 'drso_pod_drso', ls_tipolinea)
NEXT
idw_detalles.SetReDraw(True)

end subroutine

public subroutine wf_filtraciudad ();/*
Name: wf_filtraCiudad()
*/
DataWindowChild ldw_child
Long ll_codigoPais


// Ciudad
IF idw_control.GetRow() > 0 THEN
	idw_control.SetReDraw(False)
	ll_codigoPais = idw_control.GetItemNumber(idw_control.GetRow(), 'pais')
	IF IsNull(ll_codigoPais) THEN ll_codigoPais = 0
		
	IF idw_control.GetChild("sord_cod_ciud", ldw_child) = 1 THEN
		ldw_child.SetFilter("ciud_cod_pais = " + String(ll_codigoPais))
		ldw_child.Filter()
	END IF
	idw_control.SetReDraw(True)
END IF
end subroutine

public subroutine wf_filtracuartofrio (integer ai_valordefault);/*

PRMS:
	ai_valorDefault -> 0: Solo filtra, 1: pone el valor del c. frio si hay solo uno
*/
Long ll_codigoAgenciaCarga, ll_codigoCuartoFrio
datawindowchild ldw_child


IF idw_control.GetRow() > 0 THEN
	idw_control.SetReDraw(False)
	ll_codigoAgenciaCarga = idw_control.GetItemNumber(idw_control.GetRow(),"sord_cod_acar")
	IF IsNull(ll_codigoAgenciaCarga) THEN ll_codigoAgenciaCarga = 0

	IF idw_control.GetChild("sord_cod_cfri", ldw_child) = 1 THEN
		ldw_child.SetFilter("cfri_cod_acar = " + String(ll_codigoAgenciaCarga))
		ldw_child.Filter()
		
		IF ldw_child.RowCount() = 1 AND ai_valorDefault = 1 THEN
			ll_codigoCuartoFrio = ldw_child.GetItemNumber(1, 'cfri_cod_cfri')
			idw_control.Object.sord_cod_cfri[idw_control.GetRow()] = ll_codigoCuartoFrio
		END IF
	END IF
	idw_control.SetReDraw(True)
END IF

end subroutine

public subroutine wf_filtra_subclientes ();DataWindowChild ldw_child


dw_1.SetReDraw(False)
IF dw_1.GetRow() > 0 THEN
	idw_control.GetChild("sord_cod_scli", ldw_child)
	ldw_child.SetTransObject(sqlca)
	ldw_child.Retrieve(il_empresa, il_cliente)
END IF
dw_1.SetReDraw(True)

end subroutine

public function integer wf_validacaja (long al_codigocajaorigen, long al_codigocaja);/*
Name: wf_validaCaja
	Valida la caja al ser modificada
*/
String ls_tipoCajaOrigen, ls_tipoCaja
Long ll_cajaIniAct, ll_cajaIniSig
dwItemStatus l_status


IF idw_detalles.GetRow() <> idw_detalles.RowCount() THEN
	ll_cajaIniAct = idw_detalles.GetItemNumber(idw_detalles.GetRow(), 'drso_caj_inic')
	ll_cajaIniSig = idw_detalles.GetItemNumber(idw_detalles.GetRow() + 1, 'drso_caj_inic')
	
	IF ll_cajaIniAct = ll_cajaIniSig THEN
		l_status = idw_detalles.GetItemStatus(idw_detalles.GetRow(), 0, Primary!)
		CHOOSE CASE l_status
			CASE DataModified!
				SELECT caja_tip_caja INTO :ls_tipoCaja
				FROM saecaja
				WHERE caja_cod_caja = :al_codigoCaja;
				
				SELECT caja_tip_caja INTO :ls_tipoCajaOrigen
				FROM saecaja
				WHERE caja_cod_caja = :al_codigoCajaOrigen;
				
				IF ls_tipoCajaOrigen = 'M' AND ls_tipoCaja = 'S' THEN
					MessageBox('Error', 'Tipo de caja no puede ser cambiado')
					Return -1
				END IF
		END CHOOSE
	END IF
END IF

Return 1
end function

public subroutine wf_filtra_notify (integer ai_default);DataWindowChild ldw_child
String ls_subcliente, ls_notify
Long ll_codigoAgenciaCarga, ll_codigoCuartoFrio, ll_codigoPais, ll_codigoCiudad


dw_1.SetReDraw(False)
IF dw_1.GetRow() > 0 THEN
	ls_subcliente = Trim(dw_1.GetItemString(dw_1.GetRow(), 'sord_cod_scli'))
	
	IF NOT IsNull(ls_subcliente) THEN
		idw_control.GetChild("sord_cod_nosc", ldw_child)
		ldw_child.SetTransObject(sqlca)
		ldw_child.Retrieve(il_empresa, il_cliente, ls_subcliente)
		
		IF ldw_child.RowCount() > 0 THEN
			ls_notify = dw_1.GetItemString(dw_1.GetRow(), 'sord_cod_nosc')
			IF IsNull(ls_notify) THEN
				ls_notify = ldw_child.GetItemString(1, 'nosc_cod_nosc')
				dw_1.SetItem(dw_1.GetRow(), 'sord_cod_nosc', ls_notify)
			END IF
		END IF
		
		// Defaults por subcliente
		IF ai_default = 1 THEN
			SELECT scli_cod_acar, scli_cod_cfri, scli_cod_pais, scli_cod_ciud
			INTO :ll_codigoAgenciaCarga, :ll_codigoCuartoFrio, :ll_codigoPais, :ll_codigoCiudad
			FROM saescli
			WHERE scli_cod_empr = :il_empresa
			AND scli_cod_clpv = :il_cliente
			AND scli_cod_scli = :ls_subcliente;
			
			dw_1.SetItem(dw_1.GetRow(), 'sord_cod_acar', ll_codigoAgenciaCarga)
			wf_filtraCuartoFrio(0)
			dw_1.SetItem(dw_1.GetRow(), 'sord_cod_cfri', ll_codigoCuartoFrio)
			
			dw_1.SetItem(dw_1.GetRow(), 'pais', ll_codigoPais)
			wf_filtraCiudad()
			dw_1.SetItem(dw_1.GetRow(), 'sord_cod_ciud', ll_codigoCiudad)
		END IF
	END IF
END IF
dw_1.SetReDraw(True)

end subroutine

public subroutine wf_actualizapuntocortedetalle ();/*
Name: wf_actualizaPuntoCorteDetalle
	Actualiza el punto de corte en el detalle
*/
Long ll_cont


FOR ll_cont = 1 TO idw_detalles.RowCount()
	idw_detalles.SetItem(ll_cont, 'drso_cod_ptco', il_puntocorte)
NEXT


end subroutine

public subroutine wf_filtrarvariedades (long al_codigofinca);/*
Name: wf_filtrarVariedades

Create: 23-04-2018
Autor: Juan Pablo
*/
DataWindowChild ldw_child


dw_deta.SetReDraw(False)
IF dw_deta.GetChild("drso_cod_vard", ldw_child) = 1 THEN
	ldw_child.SetFilter("vard_cod_finc = " + String(al_codigoFinca) + &
		" and vard_ori_vard like '" + is_origenFlor + "'")
	ldw_child.Filter()
END IF
dw_deta.SetReDraw(True)

end subroutine

public subroutine wf_suprimeopciones ();Long ll_codigoOrdenFija
Long ll_codigoEmpresa
Date lda_fechaServidor
Long ll_rows
Boolean lb_detalle


IF idw_control.GetRow() > 0 THEN
	ll_codigoOrdenFija = idw_control.GetItemNumber(idw_control.GetRow(), 'sord_cod_sord')
	ll_codigoEmpresa   = idw_control.GetItemNumber(idw_control.GetRow(), 'sord_cod_empr')
	lda_fechaServidor  = f_fecha_server()
	
	SELECT Count(*) INTO :ll_rows
	FROM saeprpa
	WHERE prpa_cod_sord = :ll_codigoOrdenFija
	AND prpa_cod_empr   = :ll_codigoEmpresa
	AND prpa_fec_prpa  >= :lda_fechaServidor;

	IF ll_rows > 0 THEN
		// EDICION
		im_menu_principal[ii_numero_menu].m_edicion.m_eliminar.Enabled = False
		im_menu_principal[ii_numero_menu].m_edicion.m_grabar.Enabled = False
		
		lb_detalle = False
	ELSE
		lb_detalle = True
	END IF
	
	// DETALLES
	im_menu_principal[ii_numero_menu].m_detalles.m_insertar.Enabled = lb_detalle
	im_menu_principal[ii_numero_menu].m_detalles.m_insertar.Visible = lb_detalle
	im_menu_principal[ii_numero_menu].m_detalles.m_insertar.ToolBarItemVisible = lb_detalle
	
	im_menu_principal[ii_numero_menu].m_detalles.m_eliminardetalle.Enabled = lb_detalle
	im_menu_principal[ii_numero_menu].m_detalles.m_eliminardetalle.Visible = lb_detalle
	im_menu_principal[ii_numero_menu].m_detalles.m_eliminardetalle.ToolBarItemVisible = lb_detalle
END IF

end subroutine

public subroutine wf_filtracajamarcada ();/*
wf_filtraCajaMarcada
11-06-2019
*/
DataWindowChild ldw_child
Long ll_codigoCliente


// Cabecera
IF idw_control.GetRow() > 0 THEN
	idw_control.SetReDraw(False)
	ll_codigoCliente = idw_control.GetItemNumber(idw_control.GetRow(), 'sord_cod_clpv')
	IF IsNull(ll_codigoCliente) THEN ll_codigoCliente = 0
		
	IF idw_control.GetChild("sord_cod_cmar", ldw_child) = 1 THEN
		ldw_child.setfilter("cmar_cod_empr="+string(gnv_ctrdw.il_empresa)+ &
			" and cmar_cod_clpv = " + String(ll_codigoCliente))
		ldw_child.Filter()
	END IF
	idw_control.SetReDraw(True)
END IF

// Detalle
IF idw_detalles.GetRow() > 0 THEN
	idw_detalles.SetReDraw(False)
	ll_codigoCliente = idw_control.GetItemNumber(idw_control.GetRow(), 'sord_cod_clpv')
	IF IsNull(ll_codigoCliente) THEN ll_codigoCliente = 0
		
	IF idw_detalles.GetChild("drso_cod_cmar", ldw_child) = 1 THEN
		ldw_child.setfilter("cmar_cod_empr="+string(gnv_ctrdw.il_empresa)+ &
			" and cmar_cod_clpv = " + String(ll_codigoCliente))
		ldw_child.Filter()
	END IF
	idw_detalles.SetReDraw(True)
END IF

end subroutine

on w_m_preventa_finca.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_menu_main_ingreso_d2" then this.MenuID = create m_menu_main_ingreso_d2
this.cb_copy=create cb_copy
this.dw_imprimir=create dw_imprimir
this.dw_deta=create dw_deta
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_copy
this.Control[iCurrent+2]=this.dw_imprimir
this.Control[iCurrent+3]=this.dw_deta
this.Control[iCurrent+4]=this.dw_1
end on

on w_m_preventa_finca.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_copy)
destroy(this.dw_imprimir)
destroy(this.dw_deta)
destroy(this.dw_1)
end on

event ue_postopen;call super::ue_postopen;DataWindowChild ldw_child
String ls_formapreventa


il_codigoFinca = gi_finca	// 23-04-2018

// main ingeso
im_menu_principal[ii_numero_menu] = m_menu_main_ingreso_d2
dw_imprimir.SetTransObject(sqlca)
dw_imprimir.Object.DataWindow.Zoom = 92
dw_imprimir.visible=false

idw_control = dw_1
idw_detalles = dw_deta

// imprimir
im_menu_principal[ii_numero_menu].m_archivo.m_imprimir.enabled = true
im_menu_principal[ii_numero_menu].m_archivo.m_imprimir.toolbaritemvisible = true
m_menu_main_ingreso_d2.m_detalles.m_inventario.Visible = False
m_menu_main_ingreso_d2.m_detalles.m_inventario.ToolBarItemVisible = False

// filtros
idw_control.GetChild("sord_cod_clpv", ldw_child)
ldw_child.SetFilter("clpv_cod_empr = " + String(gnv_ctrdw.il_empresa) + &
	" and grpv_tip_dest = 'E' and clpv_est_clpv = 'A'")
ldw_child.Filter()

// 29-06-2017
ifiltros_generales.of_setCodigoEmpresa(gnv_ctrdw.il_empresa)
ifiltros_generales.of_setCodigoFinca(gi_finca)
ifiltros_generales.of_puntoCorte(idw_control, 'sord_cod_ptco', '03')
ifiltros_generales.of_envoltura(idw_control, 'sord_cod_envo', '03')

ifiltros_generales.of_tiposCaja(idw_control, 'sord_cod_tcan', '01')


// Filtra detalles
// 29-06-2017
ifiltros_generales.of_tallosxRamo(dw_deta, 'drso_cod_ntal', '02')
ifiltros_generales.of_tamanioCaja(dw_deta, 'drso_cod_caja', '01')

ifiltros_generales.of_setCodigoFinca(gi_finca)
ifiltros_generales.of_grado(dw_deta, 'drso_num_grad', '02')


is_manejaFlorComprada = iparametros_floricolas.of_getManejaFlorComprada(gi_finca)
IF is_manejaFlorComprada = 'S' THEN is_origenFlor = '%'

wf_filtrarVariedades(gi_finca)	// 23-04-2018

// Reporte
ifiltros_generales.of_puntoCorte(dw_imprimir, 'sord_cod_ptco', '01')


// Secuencial
ls_formapreventa = iparametros_floricolas.of_getformapreventa(gi_finca)
IF ls_formapreventa = 'M' THEN
	dw_1.Modify("sord_sec_sord.protect = 0")
	dw_1.Modify("sord_sec_sord.background.color = rgb(255, 255, 255)")
ELSE
	dw_1.Modify("sord_sec_sord.protect = 1")
//	dw_1.Modify("sord_sec_sord.background.color = 80269528")
	dw_1.Modify("sord_sec_sord.background.color = '80269528~tf_color(0)'")	// 30-07-2018
END IF

il_empresa = gnv_ctrdw.il_empresa

// 24-05-2018
IF dw_deta.GetChild("drso_cod_vard_1", ldw_child) = 1 THEN
	ldw_child.SetFilter("vard_ori_vard like '" + is_origenFlor + "'")
	ldw_child.Filter()
END IF

// 26-11-2018
idw_control.getchild("sord_cod_temv",ldw_child)
ldw_child.setfilter("temv_cod_empr = " + String(gnv_ctrdw.il_empresa))
ldw_child.filter()

end event

event ue_borra_detalle;int i
dwItemStatus l_status


IF idw_detalles.RowCount() <> 0 THEN
	idw_detalles.accepttext()
	l_status = idw_detalles.GetItemStatus(idw_detalles.getrow(), 0, Primary!)
	choose case l_status
		case New!,NewModified!
			idw_detalles.deleterow(idw_detalles.getrow())
		case DataModified!,Notmodified!
			idw_detalles.TriggerEvent('ue_eliminar')
	end choose
	wf_barra_detalle()
END IF

/*
IF idw_detalles.RowCount() <> 0 THEN
	idw_detalles.accepttext()
	l_status = idw_detalles.GetItemStatus(idw_detalles.getrow(), 0, Primary!)
	choose case l_status
		case New!,NewModified!
 		 	idw_detalles.TriggerEvent('ue_eliminar')
		case DataModified!,Notmodified!
	 		idw_detalles.TriggerEvent('ue_eliminar')
			wf_barra_detalle() 
	end choose
END IF



*/
end event

event ue_imprimir;Integer li_retorno, li_filas
Long ll_factura
nvo_imprimir limprimir


IF dw_1.RowCount() > 0 THEN
	ll_factura = dw_1.getitemnumber(dw_1.getrow(),"sord_cod_sord")
	IF NOT IsNull(ll_factura) THEN
		dw_imprimir.Retrieve(gnv_ctrdw.il_empresa,gi_finca,ll_factura)
		li_filas = dw_imprimir.RowCount()
		IF li_filas > 0 THEN
			li_retorno = limprimir.of_imprimir( dw_imprimir )
			IF li_retorno < 0 THEN
				MessageBox ( "Error", "Funcion de impresion" )
				RETURN
		   END IF
	   END IF
	END IF
END IF

end event

event ue_integrar;call super::ue_integrar;Long ll_codigoEmpresa, ll_codigoCliente
String ls_nombreCM
nvo_packing lpacking


Open(w_res_cajamarcada)
ls_nombreCM = Message.StringParm
IF NOT IsNull(ls_nombreCM) AND ls_nombreCM <> '' THEN
	ll_codigoEmpresa = idw_control.GetItemNumber(idw_control.GetRow(), 'sord_cod_empr')
	ll_codigoCliente = idw_control.GetItemNumber(idw_control.GetRow(), 'sord_cod_clpv')
	
	lpacking.of_creaCajaMarcada(ll_codigoEmpresa, ll_codigoCliente, idw_detalles, ls_nombreCM)
END IF
end event

type cb_copy from commandbutton within w_m_preventa_finca
integer x = 3529
integer y = 92
integer width = 343
integer height = 100
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Copiar"
end type

event clicked;nvo_ordenesfijas_preventas lordenesfijas_preventas
String ls_fecha
Date lda_fechaActual
Date lda_fechaNew
Long ll_codigoOrden
Long ll_codigoEmpresa


IF dw_1.GetRow() > 0 THEN
	// Lee datos
	lda_fechaActual  = dw_1.GetItemDate(dw_1.GetRow(), 'sord_fec_sord')
	ll_codigoOrden   = dw_1.GetItemNumber(dw_1.GetRow(), 'sord_cod_sord')
	ll_codigoEmpresa = dw_1.GetItemNumber(dw_1.GetRow(), 'sord_cod_empr')

	IF IsNull(ll_codigoOrden) THEN Return
	
	// Ingresa nueva fecha
	Open(w_base_ingresodato)
	
	ls_fecha = Message.StringParm
	IF IsNull(ls_fecha) OR ls_fecha = '' THEN Return
	lda_fechaNew = Date(ls_fecha)

	// valida datos
	IF lda_fechaActual > lda_fechaNew THEN
		MessageBox('Error', 'La fecha ingresada no puede ser menor a la de la pre-venta.')
		Return
	END IF

	lordenesfijas_preventas.Trigger Event ue_copiarPreventa(ll_codigoOrden, ll_codigoEmpresa, lda_fechaNew)
END IF

end event

type dw_imprimir from datawindow within w_m_preventa_finca
integer x = 1193
integer y = 2068
integer width = 146
integer height = 76
integer taborder = 70
string title = "none"
string dataobject = "d_imp_standing_order"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_deta from uo_control_det within w_m_preventa_finca
event ue_modificar_inicial pbm_custom46
event ue_fila_anterior pbm_custom56
integer x = 9
integer y = 736
integer width = 4416
integer height = 1252
integer taborder = 20
string title = "d_m_detalle_ramo_standing"
string dataobject = "dwo_a_detalle_preventa"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;String ls_nulo ; SetNull(ls_nulo)
Long ll_caja_inicial, ll_numerocajas, ll_caja_final, ll_cajasentregadas
Long ll_codigoCajaOrigen, ll_codigoCaja


CHOOSE CASE dwo.name
	CASE "drso_caj_inic"
		IF IsNull(data) THEN Return 0
		ll_caja_inicial = Long(data)
		
		IF row = 1 THEN
			IF ll_caja_inicial <> 1 THEN
				MessageBox('Error','Caja inicial debe ser uno')
				Return 1
			END IF
		ELSE
			ll_caja_final = This.GetItemNumber(row - 1, 'drso_caj_fina')
			IF ll_caja_inicial <> ll_caja_final + 1 THEN
				MessageBox('Error','Caja inicial debe ser ' + String(ll_caja_final + 1))
				Return 1
			END IF
		END IF
		
		ll_numerocajas = This.GetItemNumber(row, 'drso_num_caja')
		ll_caja_final = ll_numerocajas + ll_caja_inicial - 1
		
		This.Object.drso_pod_drso[row] = 'P'
		This.Object.drso_caj_fina[row] = ll_caja_final
		
		
		This.Object.drso_num_caja[row] = 1	// 16-05-2018
		// 30-07-2018
		ll_caja_final = ll_numerocajas + ll_caja_inicial - 1
		
		This.Object.drso_caj_fina[row] = ll_caja_final
		
		Post wf_llena_detalles(row)
		Post wf_actualiza_totales(row)
		Post wf_renumerar()
		
	CASE "drso_cod_cmar"
		This.Object.drso_emp_come[row] = gnv_ctrdw.il_empresa
		Post wf_llena_detalles(row)
	CASE "drso_cod_tram"
		This.Object.drso_cod_ramo[row] = ls_nulo
		This.Object.drso_cod_vard[row] = ls_nulo
	CASE "drso_can_drso","drso_pre_tall"
		Post wf_actualiza_totales(row)
	CASE "drso_cod_vard","drso_num_grad", "drso_cod_vard_1"	// 20-12-2018
		This.Object.drso_ord_drso[row] = row
		Post wf_crea_ramo()
	CASE "drso_cod_ntal"
		Post wf_actualiza_totales(row)
		Post wf_crea_ramo()
	CASE "drso_num_caja"
		ll_numerocajas = Long(data)

		IF ll_numerocajas <= 0 THEN
			MessageBox('Error','Número de cajas debe ser mayor que cero')
			Return 1
		END IF
		ll_cajasentregadas = This.GetItemNumber(row, 'drso_can_entr')
		IF ll_numerocajas <= ll_cajasentregadas THEN
			MessageBox('Error','Número de cajas debe ser mayor que las cajas entregadas')
			Return 1
		END IF
		
		ll_caja_inicial = This.GetItemNumber(row, 'drso_caj_inic')
		ll_caja_final = ll_numerocajas + ll_caja_inicial - 1
		
		This.Object.drso_caj_fina[row] = ll_caja_final
		
		Post wf_llena_detalles(row)
		Post wf_actualiza_totales(row)
		Post wf_renumerar()
	CASE "drso_cod_caja"
		// Valida si la caja es mixta
		ll_codigoCaja = Long(data)
		ll_codigoCajaOrigen = This.GetItemNumber(row, 'drso_cod_caja', Primary!, True)
		
		IF wf_validaCaja(ll_codigoCajaOrigen, ll_codigoCaja) = -1 THEN
			Return 1
		ELSE
			Post wf_llena_detalles(row)
		END IF
	CASE "drso_cod_finc"
		il_codigoFinca = Long(data)
		
		IF ivalidainformacion.of_verificarFincaPorCaja(This, row, il_codigoFinca) = 1 THEN Return 1
		
		Post wf_filtrarVariedades(il_codigoFinca)	// 23-04-2018
	CASE 'drso_pre_tfin'	// 09-07-2018
		IF iflete.of_setPrecioTallo(Double(data), dw_deta) = 1 THEN Return 1
		Post wf_actualiza_totales(row)
END CHOOSE

end event

event ue_nuevo;Long ll_inicial, ll_final, ll_cantidad, ll_cajas, ll_marca, ll_tipo_caja
Char ls_tipo_caja, ls_x
String ls_envoltura, ls_ramo, ls_solida
Long ll_habilitar, ll_corte, ll_cliente, ll_cantidadentregada, ll_new
nvo_funciones_facturacion lfunciones_facturacion


// Valida que no ingrese lineas intermedias en cajas mixtas
IF This.GetRow() < This.RowCount() THEN
	ls_x = This.GetItemString(This.GetRow() + 1, "drso_pod_drso")
	IF ls_x = 'D' THEN
		MessageBox('Advertencia', 'No puede ingresar líneas intermedias en cajas mixtas.', Exclamation!)
		Return -1
	END IF
END IF


// Verifica detalles
IF idw_control.GetRow() > 0 THEN
	ll_corte		= idw_control.GetItemNumber(idw_control.GetRow(),"sord_cod_ptco")
	ls_envoltura= idw_control.GetItemString(idw_control.GetRow(),"sord_cod_envo")
END IF

IF This.RowCount() > 0 AND This.GetRow() > 0 THEN
	ll_inicial	= This.GetItemNumber(This.GetRow(), "drso_caj_inic")
	ll_final		= This.GetItemNumber(This.GetRow(), "drso_caj_fina")
	ll_cajas		= This.GetItemNumber(This.GetRow(), "drso_num_caja")
	ll_tipo_caja= This.GetItemNumber(This.GetRow(), "drso_cod_caja")
	ll_marca		= This.GetItemNumber(This.GetRow(), "drso_cod_cmar")
	ls_x 			= This.GetItemString(This.GetRow(), "drso_pod_drso")
	ll_cantidadentregada = This.GetItemNumber(This.GetRow(), "drso_can_entr")
	il_codigoFinca = This.GetItemNumber(This.GetRow(), "drso_cod_finc")

	ls_solida = lfunciones_facturacion.of_gettipocaja(ll_tipo_caja)
	CHOOSE CASE ls_solida
		CASE 'S'
			ll_inicial++
			ll_final = 0
			ll_cajas = 0
			ll_cantidadentregada = 0
			ls_x = 'P'
		CASE 'M'
			ls_x = 'D'
	END CHOOSE

	// Valores iniciales
	This.Object.drso_caj_inic.Initial = String(ll_inicial)
	This.Object.drso_caj_fina.Initial = String(ll_final)
	This.Object.drso_num_caja.Initial = String(ll_cajas)
	This.Object.drso_emp_come.Initial = String(gnv_ctrdw.il_empresa)
	This.Object.drso_can_entr.Initial = String(ll_cantidadentregada)
	This.Object.drso_pod_drso.Initial = ls_x
	This.Object.drso_cod_finc.Initial = String(il_codigoFinca)	// 23-04-2018
ELSE
	SetNull(ll_tipo_caja)
//	SetNull(ll_marca)
	ll_marca = idw_control.getitemnumber(idw_control.getrow(), "sord_cod_cmar")	// 11-06-2019
	This.Object.drso_caj_inic.Initial = String(1)
	This.Object.drso_caj_fina.Initial = String(0)
	This.Object.drso_num_caja.Initial = String(0)
	This.Object.drso_emp_come.Initial = String(gnv_ctrdw.il_empresa)
	This.Object.drso_can_entr.Initial = String(0)
	This.Object.drso_pod_drso.Initial = 'P'
	This.Object.drso_cod_finc.Initial = String(il_codigoFinca)	// 23-04-2018
END IF
This.Object.drso_cod_ptco.Initial = String(ll_corte)
This.Object.drso_cod_envo.Initial = ls_envoltura

wf_filtrarVariedades(il_codigoFinca)	// 23-04-2018

// Crea nuevo registro
ll_new = This.GetRow()
IF This.GetRow() = 1 THEN
	IF MessageBox('Aviso', 'Inserta la linea antes', Question!, YesNo!, 2) = 1 THEN
		ll_new = 0
		SetNull(ll_tipo_caja)
		SetNull(ll_marca)
		This.Object.drso_caj_inic.Initial = String(1)
		This.Object.drso_caj_fina.Initial = String(0)
		This.Object.drso_num_caja.Initial = String(0)
		This.Object.drso_emp_come.Initial = String(gnv_ctrdw.il_empresa)
		This.Object.drso_can_entr.Initial = String(0)
		This.Object.drso_pod_drso.Initial = 'P'
	END IF
END IF

This.Object.drso_cod_caja.Initial = String(ll_tipo_caja)	// 24-05-2018
This.Object.drso_cod_cmar.Initial = String(ll_marca)	// 24-05-2018


gnv_ctrdw.ii_fila = InsertRow(ll_new + 1)
ScrollToRow(gnv_ctrdw.ii_fila)

IF NOT IsNull(is_primera_columna) THEN SetColumn(is_primera_columna)
Modify("DataWindow.HorizontalScrollPosition = 0")
//wf_numera_filas()	// 24-05-2018

Return 0		

end event

event dberror;//
end event

event ue_eliminar;This.DeleteRow(0)
wf_numera_filas()
wf_renumerar()
end event

event itemerror;call super::itemerror;CHOOSE CASE dwo.name
	CASE "drso_caj_inic"
		Return 1
	CASE "drso_num_caja", "drso_cod_finc"
		Return 1
	CASE "drso_pre_tfin"
		Return 1
END CHOOSE

end event

event rowfocuschanged;IF This.RowCount() > 0 THEN
	wf_actualiza_totales(currentrow)
	
	il_codigoFinca = This.GetItemNumber(currentrow, 'drso_cod_finc')
	Post wf_filtrarVariedades(il_codigoFinca)	// 23-04-2018
END IF

end event

event ue_grabar;/*
Nombre: ue_grabar
	Graba los cambios de los detalles
Retorno
	0: Fallo
	1: Exito
*/
dwItemStatus l_status


IF AcceptText() = -1 THEN Return 0	// 13-07-2018

l_status = This.GetItemStatus(This.GetRow(), "drso_num_caja", Primary!)
IF l_status = DataModified! THEN
	wf_llena_detalles(This.GetRow())
	wf_actualiza_totales(This.GetRow())
//		wf_renumerar()
END IF
wf_renumerar()	// 13-07-2018

IF uf_verifica_datos() THEN
	IF UpDate(True,False) = 1 THEN
		//Commit;
		ResetUpdate()
		RETURN 1
	ELSE
//		MessageBox('Error','No se pudo actualizar la base')
		RollBack;
		RETURN 0
	END IF
	// Menu
//	gnv_ctrdw.of_run_even_wind(Parent,'ue_menu',0,0)
ELSE
	RETURN 0
END IF

end event

event doubleclicked;call super::doubleclicked;CHOOSE CASE dwo.name
	CASE 'drso_pre_tall'
		iflete.of_copiaPrecioUnitario(dw_deta, 'drso_pre_tall', 'drso_num_grad')
	CASE 'drso_pre_tfin'
		iflete.of_copiaPrecioUnitario(dw_deta)	// 09-07-2018
		Post wf_actualiza_totales(0)
END CHOOSE

end event

event ue_tabtoenter;Long ll_fila


// Verifica si tiene flete
String ls_estadoFlete
String ls_nombreCampo = 'drso_pre_tall'


ls_estadoFlete = idw_control.GetItemString(idw_control.GetRow(), 'sord_est_flet')
IF ls_estadoFlete = 'S' THEN ls_nombreCampo = is_ultima_columna
	
If GetColumnName() = ls_nombreCampo then
	ll_fila = GetRow() 
	If ll_fila = RowCount() Then
		TriggerEvent('ue_nuevo')
		Return 0
	End If
End If

Send(Handle(This),256,9,Long(0,0))
Return 1
end event

event rbuttondown;call super::rbuttondown;Window lw_padre, lw__mdi
Integer li_posicion_x, li_posicion_y
m_submenu lm_nuevo
dwItemStatus l_status


CHOOSE CASE dwo.name
	CASE 'drso_cod_cmar'
		l_status = This.GetItemStatus(row, 0, Primary!)
		IF l_status = New! OR l_status = NewModified! THEN
			lw_padre = GetParent()
			lw__mdi = lw_padre.ParentWindow()
			
			li_posicion_x = lw__mdi.Pointerx()
			li_posicion_y = lw__mdi.Pointery()
			
			lm_nuevo = Create m_submenu
			lm_nuevo.m_cajamarcada.PopMenu(li_posicion_x, li_posicion_y)
			Destroy lm_nuevo
		END IF
END CHOOSE

end event

type dw_1 from uo_control_cabe within w_m_preventa_finca
integer x = 9
integer y = 8
integer width = 3374
integer height = 728
string dataobject = "dwo_a_preventa"
end type

event dberror;//
end event

event rowfocuschanged;call super::rowfocuschanged;DataWindowChild ldw_child


IF This.GetRow() > 0 AND NOT ib_buscar THEN
/*
	Long ll_rows, ll_codigoSO
	Date lda_fecha
	
	
	ll_codigoSO = This.GetItemNumber(This.GetRow(), 'sord_cod_sord')
	lda_fecha   = f_fecha_server()
	
	SELECT Count(prpa_cod_sord) INTO : ll_rows
	FROM saeprpa
	WHERE prpa_cod_sord = :ll_codigoSO
	AND prpa_cod_finc = :gi_finca
	AND prpa_cod_empr = :gnv_ctrdw.il_empresa
	aND prpa_fec_prpa >= :lda_fecha;

	IF ll_rows > 0 THEN
		MessageBox('Aviso', 'Registro no puede ser modificado, tiene pre-packing.')
	END IF
	*/
//
	il_puntocorte = This.GetItemNumber(This.GetRow(), 'sord_cod_ptco')
	
	il_cliente = This.GetItemNumber(This.GetRow(), 'sord_cod_clpv')

	wf_filtra_subclientes()
	wf_filtra_notify(0)
	wf_filtraCiudad()
	wf_filtraCuartoFrio(0)
	wf_filtraCajaMarcada()
END IF

//messagebox(string(ib_buscar), string(ib_nuevo) + ' ' + string(dw_1.RowCount()))

IF dw_1.RowCount() > 0 AND NOT ib_buscar AND NOT ib_nuevo THEN
	cb_copy.Enabled = True	// 31-10-2018
ELSE
	cb_copy.Enabled = False	// 31-10-2018
END IF

end event

event ue_nuevo;This.Object.sord_cod_empr.Initial = String(gnv_ctrdw.il_empresa)
This.Object.sord_cod_sucu.Initial = String(gnv_ctrdw.il_sucursal)
This.Object.sord_cod_finc.Initial = String(gi_finca)
This.Object.sord_emp_come.Initial = String(gnv_ctrdw.il_empresa)
This.Object.sord_fec_sord.Initial = String(gnv_ctrdw.ida_fecha_sistema)
//This.Object.sord_sec_sord.Initial = String(iparametros_floricolas.of_getsecuencialpreventa(gi_finca))

// 26-11-2018
nvo_parametros_floricolas lparametros_floricolas
Long ll_codigoVentaTemporada


ll_codigoVentaTemporada = lparametros_floricolas.of_getpreventaventatemporada ( gi_finca )
This.Object.sord_cod_temv.Initial = String(ll_codigoVentaTemporada)
//setitem(getrow(),"sord_cod_temv", ll_codigoVentaTemporada)

CALL Super::ue_nuevo

end event

event ue_eliminar;if dw_deta.rowcount()>0 then
	messagebox("Error","No se puede eliminar existen detalles")
else
	il_factura =getitemnumber(getrow(),"sord_cod_sord")
	delete from saedord
	where dord_cod_finc=:gi_finca
	and dord_cod_sord=:il_factura;
	commit;
	DeleteRow(ii_row)	
	TriggerEvent('ue_grabar')
	reset()
	ii_boton = 0
	uf_actualiza_barra_menu()
	ib_nuevo = False
end if

end event

event ue_graba;Long ll_secuencial, ll_rows, ll_codigoPuntoCorte


AcceptText()
gl_valor=0
IF uf_verifica_datos() THEN
	IF ib_nuevo THEN
		ll_secuencial = iparametros_floricolas.of_getsecuencialpreventa(gi_finca)
		This.SetItem(This.GetRow(), 'sord_sec_sord', ll_secuencial)
		/*
		ll_secuencial = This.GetItemNumber(This.GetRow(), 'sord_sec_sord')
		
		SELECT Count(sord_cod_sord) INTO :ll_rows
		FROM saesord
		WHERE sord_cod_finc = :gi_finca
		AND sord_tip_sord = 'P'
		AND sord_sec_sord = :ll_secuencial;
		
		IF ll_rows = 0 THEN
		ELSE
			ll_secuencial = iparametros_floricolas.of_getsecuencialpreventa(gi_finca)
		END IF
		*/
	END IF
	
	IF UpDate(True,False) = 1 THEN
		IF ib_nuevo THEN
			iparametros_floricolas.of_setsecuencialpreventa(ll_secuencial, &
				gi_finca, False)
		END IF
//		Commit;
		ResetUpdate()
		gl_valor=1
		ib_nuevo = false
		ii_boton = 0
		uf_actualiza_barra_menu()
	ELSE
		RollBack;
	END IF
	gnv_ctrdw.of_run_even_wind(Parent,'ue_menu',0,0)
   return 0
ELSE
	Return 1
END IF

end event

event ue_buscar;call super::ue_buscar;DataWindowChild ldw_child


idw_control.GetChild("sord_cod_clpv", ldw_child)
ldw_child.SetFilter("clpv_cod_empr = " + String(gnv_ctrdw.il_empresa) + &
	" and grpv_tip_dest = 'E'")
ldw_child.Filter()


setitem(getrow(),"sord_cod_empr",gnv_ctrdw.il_empresa)
setitem(getrow(),"sord_cod_sucu",gnv_ctrdw.il_sucursal)
setitem(getrow(),"sord_cod_finc",gi_finca)
setitem(getrow(),"sord_emp_come",gnv_ctrdw.il_empresa)
setitem(getrow(),"sord_tip_sord",'P')

This.SetItem(This.GetRow(), "sord_ban_sord", "A")	// 10-11-2017

end event

event ue_tabtoenter;Send(Handle(This),256,9,Long(0,0))
Return 1
end event

event itemchanged;call super::itemchanged;DataWindowChild ldw_child
Long ll_cont
Long ll_estadoFlete = 0


CHOOSE CASE dwo.name
	CASE 'sord_cod_clpv'
		IF IsNull(data) THEN Return 0
		il_cliente = Long(data)

		IF dw_deta.RowCount() > 0 THEN
			MessageBox('Advertencia', 'Cliente no puede ser cambiado.~r~nElimine los detalles primero')
			Return 1
		END IF
		
		IF dw_deta.GetChild("drso_cod_cmar", ldw_child) = 1 THEN
			ldw_child.SetTransObject(sqlca)
			ldw_child.SetFilter("cmar_cod_clpv = " + String(il_cliente) + &
				" and cmar_cod_empr = " + String(gnv_ctrdw.il_empresa))
			ldw_child.Filter()
		END IF
		Post wf_filtra_subclientes()
		
		This.Object.sord_cod_scli[row] = gnv_ctrdw.is_nulo
		This.Object.sord_cod_nosc[row] = gnv_ctrdw.is_nulo
		This.Object.sord_cod_acar[row] = gnv_ctrdw.il_nulo
		This.Object.sord_cod_cfri[row] = gnv_ctrdw.il_nulo
		This.Object.pais[row] = gnv_ctrdw.il_nulo
		This.Object.sord_cod_ciud[row] = gnv_ctrdw.il_nulo

		This.Object.sord_cod_cmar[row] = gnv_ctrdw.il_nulo	// 11-06-2019
		Post 	wf_filtraCajaMarcada()
	CASE 'sord_cod_envo'
		FOR ll_cont = 1 TO dw_deta.RowCount()
			dw_deta.SetItem(ll_cont, 'drso_cod_envo', data)
		NEXT
	CASE "sord_cod_ptco"
		il_puntocorte = Long(data)
		Post wf_actualizaPuntoCorteDetalle()
	CASE 'pais'
		This.Object.sord_cod_ciud[row] = gnv_ctrdw.il_nulo
		
		Post wf_filtraCiudad()
	CASE 'sord_cod_acar'
		This.Object.sord_cod_cfri[row] = gnv_ctrdw.il_nulo
		
		Post wf_filtraCuartoFrio(1)
	CASE 'sord_cod_scli'
		This.Object.sord_cod_nosc[row] = gnv_ctrdw.is_nulo
		
		Post wf_filtra_notify(1)
	CASE 'sord_est_flet'	// 09-07-2018
		IF data = 'S' THEN
			ll_estadoFlete = 1
		ELSE
			This.Object.sord_val_flet[row] = 0
		END IF
		iflete.of_activaCampo(ll_estadoFlete, dw_deta)
END CHOOSE

end event

event itemerror;call super::itemerror;CHOOSE CASE dwo.name
	CASE 'sord_cod_clpv'
		Return 1
END CHOOSE
end event

event editchanged;call super::editchanged;CHOOSE CASE dwo.name
	CASE "sord_val_flet"	// 09-07-2018
		iflete.of_recalculaFletexItem(Double(data), dw_deta)
		Post wf_actualiza_totales(0)
END CHOOSE

end event

event ue_filtro;call super::ue_filtro;Long ll_estadoFlete = 0


IF This.GetRow() > 0 THEN
	// Flete
	IF This.GetItemString(This.GetRow(), "sord_est_flet") = 'S' THEN ll_estadoFlete = 1

	iflete.of_activaCampo(ll_estadoFlete, dw_deta)
	iflete.of_recalculaFletexItem(This.GetItemNumber(This.GetRow(), "sord_val_flet"), dw_deta)
END IF

end event

